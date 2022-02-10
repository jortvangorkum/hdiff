{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
module GenericTree where

import           Control.DeepSeq
import qualified Data.Map                   as M
import           Data.Maybe                 (fromMaybe)
import           Debug.Trace                (trace, traceId)
import           GHC.Generics               (Generic, Generic1)
-- import           Generics.Simplistic.Digest
import           Control.Applicative        (liftA2)
import           Control.Monad.State
import           Generics.Data.Digest.CRC32

newtype Fix f = In { unFix :: f (Fix f) }

instance Eq (f (Fix f)) => Eq (Fix f) where
  f == g = unFix f == unFix g

instance Show (f (Fix f)) => Show (Fix f) where
  show = show . unFix

cata :: Functor f => (f a -> a) -> Fix f -> a
cata alg t = alg (fmap (cata alg) (unFix t))

newtype I r         = I r                   deriving (Show, Eq)
newtype K a r       = K a                   deriving (Show, Eq)
data (:+:) f g r    = Inl (f r) | Inr (g r) deriving (Show, Eq)
newtype (:*:) f g r = Pair (f r, g r)       deriving (Show, Eq)

infixr 7 :*:
infixr 6 :+:

-- Generic Functors
instance (Functor f, Functor g) => Functor (f :+: g) where
  fmap f (Inl x) = Inl (fmap f x)
  fmap f (Inr x) = Inr (fmap f x)

instance (Functor f, Functor g) => Functor (f :*: g) where
  fmap f (Pair (x, y)) = Pair (fmap f x, fmap f y)

instance Functor I where
  fmap f (I x) = I (f x)

instance Functor (K a) where
  fmap _ (K x) = K x

-- Generic Foldable
-- https://github.com/blamario/grampa/blob/f4b97674161c6bd5e45c20226b5fb3458f942ff4/rank2classes/src/Rank2.hs#L307
instance (Foldable f, Foldable g) => Foldable (f :+: g) where
  foldMap f (Inl x) = foldMap f x
  foldMap f (Inr x) = foldMap f x

instance (Foldable f, Foldable g) => Foldable (f :*: g) where
  foldMap f (Pair (x, y)) = foldMap f x <> foldMap f y

instance Foldable (K a) where
  foldMap _ _ = mempty

instance Foldable I where
  foldMap f (I r) = f r

-- Generic Traversable
-- https://www.tweag.io/blog/2021-07-08-linear-traversable/
instance (Traversable f, Traversable g) => Traversable (f :+: g) where
  traverse f (Inl x) = Inl <$> traverse f x
  traverse f (Inr x) = Inr <$> traverse f x

instance (Traversable f, Traversable g) => Traversable (f :*: g) where
  traverse f (Pair (x, y)) = curry Pair <$> traverse f x <*> traverse f y

instance Traversable (K a) where
  traverse f (K x) = pure (K x)

instance Traversable I where
  traverse f (I r) = I <$> f r

-- Digest NFData

instance NFData Digest where
  rnf = rwhnf

-- Generic NFData
-- https://hackage.haskell.org/package/deepseq-1.4.6.1/docs/src/Control.DeepSeq.html#line-535

instance NFData (f (Fix f)) => NFData (Fix f) where
  rnf (In x) = rnf x

instance NFData r => NFData (I r) where
  rnf = rnf1

instance NFData1 I where
  liftRnf f (I x) = f x

instance (NFData a, NFData r) => NFData (K a r) where
  rnf = rnf1

instance NFData a => NFData1 (K a) where
  liftRnf _ (K x) = rwhnf x

instance (NFData1 f, NFData1 g) => NFData1 (f :+: g) where
  liftRnf f (Inl x) = liftRnf f x
  liftRnf f (Inr x) = liftRnf f x

instance (NFData1 f, NFData1 g, NFData r) => NFData ((:+:) f g r) where
  rnf = rnf1

instance (NFData1 f, NFData1 g) => NFData1 (f :*: g) where
  liftRnf f (Pair (x, y)) = liftRnf f x `seq` liftRnf f y

instance (NFData1 f, NFData1 g, NFData r) => NFData ((:*:) f g r) where
  rnf = rnf1

{-
  TREE
-}

type TreeF a = Fix (TreeFr a)

data TreeFr a r = LeafF a
               | NodeF r a r
  deriving (Functor)

class MerkelizeF f where
  merkleF :: Fix f -> Fix (f :*: K Digest)

instance (Show a) => MerkelizeF (TreeFr a) where
  merkleF (In (LeafF n)) = In $ Pair (LeafF n, K h)
    where
      h = digestConcat [digest "LeafF", digest n]
  merkleF (In (NodeF l n r)) = In $ Pair (NodeF l' n r', K h)
    where
      l' = merkleF l
      r' = merkleF r
      dig (In (Pair (x, K xh))) = xh
      hl = dig l'
      hr = dig r'
      h = digestConcat [digest "NodeF", digest n, hl, hr]

exampleTreeF :: TreeF Int
exampleTreeF = In $ NodeF (In (LeafF 1)) 2 (In (LeafF 3))

foldTree :: TreeF Int -> Int
foldTree = cata (\case
  LeafF n     -> n
  NodeF n i j -> n + i + j)

foldMerkle :: TreeF Int -> [Digest]
foldMerkle x = cata f mt
  where
    mt = merkleF x
    f (Pair (px, K h)) = case px of
      LeafF n       -> [h]
      NodeF hl n hr -> h : hl ++ hr

{-
  GENERIC TREE
-}

debugHash :: Digest -> String
debugHash h = take 5 (show (getCRC32 h))

type MerkleFix f = Fix (f :*: K Digest)
type MerkleTree a = MerkleFix (TreeGr a)

type TreeG  a = Fix (TreeGr a)

type TreeGr a = K a
             :+: ((I :*: K a) :*: I)

merkle :: MerkelizeG f => Fix f -> MerkleFix f
merkle = In . merkleG . unFix

class (Functor f) => MerkelizeG f where
  merkleG :: (MerkelizeG g) => f (Fix g) -> (f :*: K Digest) (Fix (g :*: K Digest))

instance (Show a) => MerkelizeG (K a) where
  merkleG (K x) = Pair (K x, K h)
    where
      debug = trace ("Digest K: " ++ debugHash h)
      h = digestConcat [digest "K", digest x]

instance MerkelizeG I where
  merkleG (I x) = Pair (I prevX, K h)
    where
      debug = trace ("Digest I: " ++ debugHash h)
      prevX@(In (Pair (_, K ph))) = merkle x
      h = digestConcat [digest "I", ph]

instance (MerkelizeG f, MerkelizeG g) => MerkelizeG (f :+: g) where
  merkleG (Inl x) = Pair (Inl prevX, K h)
    where
      debug = trace ("Digest Inl: " ++ debugHash h)
      (Pair (prevX, K ph)) = merkleG x
      h = digestConcat [digest "Inl", ph]
  merkleG (Inr x) = Pair (Inr prevX, K h)
    where
      debug = trace ("Digest Inr: " ++ debugHash h)
      (Pair (prevX, K ph)) = merkleG x
      h = digestConcat [digest "Inr", ph]

instance (MerkelizeG f, MerkelizeG g) => MerkelizeG (f :*: g) where
  merkleG (Pair (x, y)) = Pair (Pair (prevX, prevY), K h)
    where
      debug = trace ("Digest Pair: " ++ debugHash h)
      (Pair (prevX, K phx)) = merkleG x
      (Pair (prevY, K phy)) = merkleG y
      h = digestConcat [digest "Pair", phx, phy]

cata2 :: Functor f => (f a -> a) -> Fix f -> a
cata2 alg t = alg z
  where
    z = fmap f x
    f = cata2 alg
    x = unFix t

cataMerkle :: Functor f => (f a -> a) -> M.Map String a -> Fix (f :*: K Digest) -> a
cataMerkle alg m (In (Pair (x, K h))) = case M.lookup (debugHash h) m of
  Just a  -> a
  Nothing -> alg (fmap (cataMerkle alg m) x)

cataMerkle2 :: (Functor f, Traversable f, Show a) => (f a -> a) -> Fix (f :*: K Digest) -> State (M.Map String a) a
cataMerkle2 alg (In (Pair (x, K h)))
  = do m <- get

       case M.lookup (debugHash h) m of
        Just a  -> trace ("LOOKUP: " ++ show a) return a
        Nothing -> trace ("CALCULATE: " ++ show m)
                 $ do y <- mapM (cataMerkle2 alg) x
                      let r = alg y
                      trace ("VALUE: " ++ show r) modify (M.insert (debugHash h) r) >> return r

cataMerkle3 :: (MerkelizeG f, Functor f, Traversable f, Show a) => (f a -> a) -> M.Map String a -> Fix f -> (a, M.Map String a)
cataMerkle3 alg m t = runState (cataMerkle2 alg (merkle t)) m

cataSum2 :: TreeG Int -> (Int, M.Map String Int)
cataSum2 = cataMerkle3 cataSum M.empty
  where
    cataSum = \case
      Inl (K x)                         -> x
      Inr (Pair (Pair (I l, K x), I r)) -> l + x + r

cataSum2WithMap :: M.Map String Int -> TreeG Int -> (Int, M.Map String Int)
cataSum2WithMap = cataMerkle3 cataSum
  where
    cataSum = \case
      Inl (K x)                         -> x
      Inr (Pair (Pair (I l, K x), I r)) -> l + x + r

{-
  Nothing -> let resultaat = ... in modify (insert resultaat h) >> return resultaat

  modify :: (s -> s) -> State s ()

  cataMerkle2 :: ... -> State (M.Map String a) a

  sequence :: f (State Map a) -> State Map (f a)

  cataMerkle2 :: (Functor f, Traversable f) => (f a -> a) -> Fix (f :*: K Digest) -> State (M.Map String a) a
-}

from :: TreeF a -> TreeG a
from = cata f
  where
    f :: TreeFr a (TreeG a) -> TreeG a
    f (LeafF x)     = In $ Inl $ K x
    f (NodeF l x r) = In $ Inr $ Pair (Pair (I l, K x), I r)

exampleTreeG :: TreeG Int
exampleTreeG = from exampleTreeF

cataInt :: TreeG Int -> Int
cataInt = cata (\case
  Inl (K x)                         -> x
  Inr (Pair (Pair (I l, K x), I r)) -> l + x + r)

cataMerkleInt :: TreeG Int -> Int
cataMerkleInt x = cata f mt
  where
    mt = merkle x
    f :: (:*:) (TreeGr Int) (K Digest) Int -> Int
    f (Pair (px, _)) = case px of
      Inl (K x)                         -> x
      Inr (Pair (Pair (I l, K x), I r)) -> l + x + r

cataMerkleHash :: Show a => TreeG a -> [Digest]
cataMerkleHash x = cata f mt
  where
    mt = merkle x
    f :: (:*:) (TreeGr a) (K Digest) [Digest] -> [Digest]
    f (Pair (px, K h)) = case px of
      Inl (K x)                         -> [h]
      Inr (Pair (Pair (I l, K x), I r)) -> h : (l ++ r)
    debug h = trace ("\nDigest Cata: " ++ debugHash h)

cataMerkleMapInt :: TreeG Int -> M.Map String Int
cataMerkleMapInt x = cata f mt
  where
    mt = merkle x
    f :: (:*:) (TreeGr Int) (K Digest) (M.Map String Int) -> M.Map String Int
    f (Pair (px, K h)) = case px of
      Inl (K x)                         -> M.insert (debugHash h) x M.empty
      Inr (Pair (Pair (I l, K x), I r)) -> M.insert (debugHash h) x (l <> r)

cataMerkleMapValue :: Show a => TreeG a -> M.Map String a
cataMerkleMapValue x = cata f mt
  where
    mt = merkle x
    f :: (:*:) (TreeGr a) (K Digest) (M.Map String a) -> M.Map String a
    f (Pair (px, K h)) = case px of
      Inl (K x)                         -> M.insert (debugHash h) x M.empty
      Inr (Pair (Pair (I l, K x), I r)) -> M.insert (debugHash h) x (l <> r)

cataMerkleTree :: (Show a) => (a -> Digest -> b) -> (b -> a -> b -> Digest -> b) -> MerkleTree a -> b
cataMerkleTree leaf node = cata f
  where
    f (Pair (px, K h)) = case px of
      Inl (K x)                         -> leaf x h
      Inr (Pair (Pair (I l, K x), I r)) -> node l x r h

cataTreeG :: (Show a) => (a -> Digest -> b) -> (b -> a -> b -> Digest -> b) -> TreeG a -> b
cataTreeG leaf node x = let mt = merkle x in cataMerkleTree leaf node mt

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

cataMerkleMapFib :: MerkleTree Int -> (Int, M.Map String Int)
cataMerkleMapFib = cataMerkleTree leaf node
  where
    leaf x h = let n = fib x
               in (n, M.insert (debugHash h) n M.empty)
    node (xl, ml) x (xr, mr) h = let n = fib x + xl + xr
                                 in (n, M.insert (debugHash h) n (ml <> mr))

cataMerkleMapFibWithMap :: M.Map String Int -> MerkleTree Int -> (Int, M.Map String Int)
cataMerkleMapFibWithMap m = cataMerkleTree leaf node
  where
    leaf x h = case M.lookup (debugHash h) m of
      Nothing -> let n = fib x in (n, M.insert (debugHash h) n M.empty)
      Just n  -> (n, m)
    node (xl, ml) x (xr, mr) h = case M.lookup (debugHash h) m of
      Nothing -> let n = fib x + xl + xr in (n, M.insert (debugHash h) n (ml <> mr))
      Just n -> (n, m)

cataMerkleMap :: MerkleTree Int -> (Int, M.Map String Int)
cataMerkleMap = cataMerkleTree leaf node
  where
    leaf x h = (x, M.insert (debugHash h) x M.empty)
    node (xl, ml) x (xr, mr) h = let x' = x + xl + xr
                                 in (x', M.insert (debugHash h) x' (ml <> mr))

cataMap :: TreeG Int -> (Int, M.Map String Int)
cataMap = cataTreeG leaf node
  where
    leaf x h = (x, M.insert (debugHash h) x M.empty)
    node (xl, ml) x (xr, mr) h = let x' = x + xl + xr
                                 in (x', M.insert (debugHash h) x' (ml <> mr))

cataWithMap :: M.Map String Int -> TreeG Int -> (Int, M.Map String Int)
cataWithMap m = cataTreeG leaf node
  where
    leaf x h = (x, M.insert (debugHash h) x M.empty)
    node (xl, ml) x (xr, mr) h = case M.lookup (debugHash h) m of
      Nothing -> let x' = x + xl + xr
                 in (x', M.insert (debugHash h) x' (ml <> mr))
      Just n  -> (n, m)

cataMerkleWithMapStop :: M.Map String Int -> MerkleTree Int -> (Int, M.Map String Int)
cataMerkleWithMapStop m (In (Pair (x, K h))) = case M.lookup (debugHash h) m of
  Nothing -> y
  Just n  -> (n, m)
  where
    y = case x of
      Inl (K x) -> (x, M.insert (debugHash h) x M.empty)
      Inr (Pair (Pair (I l, K x), I r)) -> (x', m')
        where
          (xl, ml) = cataMerkleWithMapStop m l
          (xr, mr) = cataMerkleWithMapStop ml r
          x' = x + xl + xr
          m' = M.insert (debugHash h) x' mr

cataMerkleWithMap :: M.Map String Int -> MerkleTree Int -> (Int, M.Map String Int)
cataMerkleWithMap m = cataMerkleTree leaf node
  where
    leaf x h = (x, M.insert (debugHash h) x M.empty)
    node (xl, ml) x (xr, mr) h = case M.lookup (debugHash h) m of
      Nothing -> let x' = x + xl + xr
                 in (x', M.insert (debugHash h) x' (ml <> mr))
      Just n  -> (n, m)

showTreeG :: String
showTreeG = show $ merkleG $ unFix exampleTreeG

getRootDigest :: MerkleTree a -> Digest
getRootDigest (In (Pair (_, K h))) = h

genMTWithMT :: (Show a, Eq a) => MerkleTree a -> TreeG a -> (Bool, MerkleTree a)
genMTWithMT lm@(In (Pair (Inl (K y), K h)))
            l@(In (Inl (K x)))
            = if x == y
              then (True, lm)
              else (False, merkle l)
genMTWithMT nm@(In (Pair (Inr (Pair (Pair (I lm, K y), I rm)), K h)))
            n@(In (Inr (Pair (Pair (I l, K x), I r))))
            = if x == y && sl && sr
              then (True, nm)
              else (False, In (Pair (Inr (Pair (Pair (I l', K x), I r')), K h')))
            where
              (sl, l') = genMTWithMT lm l
              (sr, r') = genMTWithMT rm r
              hl = getRootDigest l'
              hr = getRootDigest r'
              h' = digestConcat [ digest "Inr", digestConcat [
                                  digest "Pair", digestConcat [
                                     digest "Pair", hl, digestConcat [
                                       digest "K", digest x
                                     ],
                                     hr
                                   ]
                                 ]
                               ]
genMTWithMT _ x = (False, merkle x)
{-
  The Digest are only saved on the Inl and Inr, because of the Fix.
  The Fix point is the point where the Digest gets saved.
  This means that only the complete type TreeG gets saved and not its children

  Output showTreeG:

  Pair (
    Inr (
      Pair (
        Pair (
          I Pair (
            Inl (K 1), K (Digest {getDigest = 54f891366fdb7609991fd43bd9fce1536585e0ee1155950dd9f4e50e86ddd00b})
          ), K 2)
        , I Pair (
          Inl (K 3), K (Digest {getDigest = 23b4fd7970f5e4d32fdbcc5229b865f4c40814c165549f1224455a9d03af9aae})
        )
      )
    ), K (Digest {getDigest = 1ea1b35d366a098365f9a55cbea71b233490af0990710adffc62bc5c9a1cefb3})
  )
-}
