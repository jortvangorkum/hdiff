{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE TypeOperators #-}
module GenericTree where

import           Generics.Simplistic.Digest

newtype Fix f = In (f (Fix f))

cata :: Functor f => (f a -> a) -> Fix f -> a
cata alg (In t) = alg (fmap (cata alg) t)

data (:+:) f g r = Inl (f r) | Inr (g r) deriving (Functor)
newtype (:*:) f g r = Pair (f r, g r)    deriving (Functor)
newtype I r = I r                        deriving (Functor)
newtype K a r = K a                      deriving (Functor)

type Tree a = Fix (TreeF a)
-- type Tree  a = Fix (TreeG a)
type TreeG a = K a
            :+: I :*: K a :*: I
data TreeF a r = LeafF a
               | NodeF r a r
  deriving (Functor)

class MerkelizeF f where
  merkleF :: Fix f -> Fix (f :*: K Digest)

class (Functor f) => MerkelizeG f where
  merkleG :: Fix f -> Fix (f :*: K Digest)

instance (Show a) => MerkelizeF (TreeF a) where
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

exampleTreeF :: Tree Int
exampleTreeF = In $ NodeF (In (LeafF 1)) 2 (In (LeafF 3))

foldTree :: Tree Int -> Int
foldTree = cata (\case
  LeafF n     -> n
  NodeF n i j -> n + i + j)

foldMerkle :: Tree Int -> [Digest]
foldMerkle x = cata f mt
  where
    mt = merkleF x
    f (Pair (px, K h)) = case px of
      LeafF n       -> [h]
      NodeF hl n hr -> h : hl ++ hr

-- GENERIC

getDig :: Fix (a :*: K Digest) -> Digest
getDig (In (Pair (_, K h))) = h

instance (Show a) => MerkelizeG (K a) where
  merkleG (In (K x)) = In $ Pair (K x, K h)
    where
      h = digestConcat [digest "K", digest x]

instance MerkelizeG I where
  merkleG (In (I r)) = In $ Pair (I x, K h)
    where
      x = merkleG r
      ph = getDig x
      h = digestConcat [digest "I", ph]

instance (MerkelizeG f, MerkelizeG g) => MerkelizeG (f :+: g) where
  merkleG (In (Inl x)) = In $ Pair (Inl x', K h)
    where
      x' = merkleG <$> x
      ph = getDig <$> x'
      h  = digestConcat [digest "Inl", ph]
  merkleG (In (Inr x)) = undefined

instance (MerkelizeG f, MerkelizeG g) => MerkelizeG (f :*: g) where
  merkleG (In (Pair (x, y))) = In $ Pair (Pair (x', y'), K h)
    where
      x' = merkleG <$> x
      y' = merkleG <$> y
      hx = getDig <$> x'
      hy = getDig <$> y'
      h = digestConcat [digest "Pair", hx, hy]


{-
  data Fix f = In (f (Fix f))

  cata :: Functor f => (f a -> a) -> Fix f -> a
  cata alg (In t) = alg (fmap (cata alg) t)

  newtype (:+:) f g a = Inl (f a) | Inr (g a)
  newtype (:*:) f g a = Pair (f a , g a)
  type constructors for :+: :*: Id en K

  schrijf type TreeF a r = K a :+: I :*: K a :*: I en type Tree = Fix Tree f

  merkle : Fix f -> Fix (f :*: K Hash)

  cataMerkle : ... verwacht een Map als input
-}
