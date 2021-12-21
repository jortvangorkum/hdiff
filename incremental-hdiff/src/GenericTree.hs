{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
module GenericTree where

import           Generics.Simplistic.Digest

newtype Fix f = In (f (Fix f))

instance Eq (f (Fix f)) => Eq (Fix f) where
  (In f) == (In g) = f == g

instance Show (f (Fix f)) => Show (Fix f) where
  show (In x) = show x

cata :: Functor f => (f a -> a) -> Fix f -> a
cata alg (In t) = alg (fmap (cata alg) t)

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

type TreeG  a = Fix (TreeGr a)

type TreeGr a = K a
            :+: ((I :*: K a) :*: I)

class (Functor f) => MerkelizeG f where
  merkleG :: (MerkelizeG g) => f (Fix g) -> (f :*: K Digest) (Fix g)

instance (Show a) => MerkelizeG (K a) where
  merkleG (K x) = Pair (K x, K h)
    where
      h = digestConcat [digest "K", digest x]

instance MerkelizeG I where
  merkleG i@(I (In x)) = Pair (I (In prevX), K h)
    where
      (Pair (prevX, K ph)) = merkleG x
      h = digestConcat [digest "I", ph]

instance (MerkelizeG f, MerkelizeG g) => MerkelizeG (f :+: g) where
  merkleG (Inl x) = Pair (Inl prevX, K h)
    where
      (Pair (prevX, K ph)) = merkleG x
      h = digestConcat [digest "Inl", ph]
  merkleG (Inr x) = Pair (Inr prevX, K h)
    where
      (Pair (prevX, K ph)) = merkleG x
      h = digestConcat [digest "Inr", ph]

instance (MerkelizeG f, MerkelizeG g) => MerkelizeG (f :*: g) where
  merkleG p@(Pair (x, y)) = Pair (Pair (prevX, prevY), K h)
    where
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

showTreeG :: String
showTreeG = show $ merkleG x
  where
    (In x) = exampleTreeG

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
