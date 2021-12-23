{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
module GenericTree where

import           Control.DeepSeq
import qualified Data.Map                   as M
import           Data.Maybe                 (fromMaybe)
import           Debug.Trace                (trace)
import           GHC.Generics               (Generic, Generic1)
import           Generics.Simplistic.Digest

newtype Fix f = In { unFix :: f (Fix f) }

instance Eq (f (Fix f)) => Eq (Fix f) where
  f == g = unFix f == unFix g

instance Show (f (Fix f)) => Show (Fix f) where
  show = show . unFix

cata :: Functor f => (f a -> a) -> Fix f -> a
cata alg t = alg (fmap (cata alg) (unFix t))

newtype I r         = I r                   deriving (Show)
newtype K a r       = K a                   deriving (Show)
data (:+:) f g r    = Inl (f r) | Inr (g r) deriving (Show)
newtype (:*:) f g r = Pair (f r, g r)       deriving (Show)

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
debugHash h = take 5 (show (getDigest h))

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
