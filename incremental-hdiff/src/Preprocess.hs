{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE UndecidableInstances #-}

module Preprocess where

import           Data.Functor.Const         (Const (..))
import           Data.HDiff.Diff.Preprocess (PrepData (PrepData), PrepFix (..))
import qualified Data.Map                   as M
import           Data.Proxy                 (Proxy (..))
import qualified Data.Text                  as T
import           Data.Word                  (Word64)
import           GHC.Generics               (Generic (Rep), V1)
import           Generics.Simplistic
import           Generics.Simplistic.Deep
import           Generics.Simplistic.Digest
import           Generics.Simplistic.Pretty (sfixAnnPretty)
import qualified Generics.Simplistic.Pretty as D
import           Generics.Simplistic.Util
import           Types
import qualified WordTrie                   as Tr

maxAlg :: forall phi f
        . (forall a . phi a -> Int)
       -> SRep phi f
       -> Int
maxAlg _ S_U1       = 0
maxAlg f (S_L1 x)   = maxAlg f x
maxAlg f (S_R1 x)   = maxAlg f x
maxAlg f (S_ST x)   = maxAlg f x
maxAlg f (S_M1 _ x) = maxAlg f x
maxAlg f (x :**: y) = max (maxAlg f x) (maxAlg f y)
maxAlg f (S_K1 x)   = f x

-- |Here we receive an expression with holes an annotate
-- it with hashes and height information at every node.
decorate :: forall kappa fam at
          . (All Digestible kappa)
         => SFix kappa fam at
         -> DecFix kappa fam at
decorate = synthesize (const onRec) (const onPrim) (const botElim)
  where
    botElim :: V1 x -> a
    botElim = error "botElim"

    pp :: Proxy kappa
    pp = Proxy

    onPrim :: (Elem b kappa) => b -> Const DecData b
    onPrim b = Const $ DecData (digPrim pp b) 0

    onRec :: SRep (Const DecData) (Rep b)
          -> Const DecData b
    onRec sr = let dig = authAlg (treeDigest . getConst) sr
                   h   = 1 + maxAlg (treeHeight . getConst) sr
                in Const $ DecData dig h

setAnn :: DecFix kappa fam ix -> DecData -> DecFix kappa fam ix
setAnn (Hole' ann x) ann' = Hole' (Const ann') x
setAnn (Prim' ann x) ann' = Prim' (Const ann') x
setAnn (Roll' ann x) ann' = Roll' (Const ann') x

decorateHash :: forall kappa fam at
          . (All Digestible kappa)
         => SFix kappa fam at
         -> DecHashFix kappa fam at
decorateHash = synthesize (const onRec) (const onPrim) (const botElim)
  where
    botElim :: V1 x -> a
    botElim = error "botElim"

    pp :: Proxy kappa
    pp = Proxy

    onPrim :: (Elem b kappa) => b -> Const DecHash b
    onPrim b = Const $ DecHash (digPrim pp b) Nothing

    onRec :: SRep (Const DecHash) (Rep b)
          -> Const DecHash b
    onRec sr = Const $ DecHash (authAlg (dig . getConst) sr) Nothing

getW64s :: Const DecData ix -> [Word64]
getW64s (Const (DecData treeDigest _)) = toW64s treeDigest

getHash :: Const DecData ix -> String
getHash (Const (DecData treeDigest _)) = show $ getDigest treeDigest

getHeight :: Const DecData ix -> Int
getHeight (Const (DecData _ treeHeight)) = treeHeight

cataP :: Monoid m =>
      (forall x . Const DecData x -> V1 x -> m)
      -> (forall x . PrimCnstr kappa fam x => Const DecData x -> x -> m)
      -> (forall x . CompoundCnstr kappa fam x => Const DecData x -> m -> m)
      -> DecFix kappa fam ix -> m
cataP f g h (Hole' ann x) = f ann x
cataP f g h (Prim' ann x) = g ann x
cataP f g h (Roll' ann x) = h ann x'
  where
    x' = repLeaves (cataP f g h) (<>) mempty x

foldPrepFixToDecDataMap :: DecFix kappa fam ix -> M.Map String DecData
foldPrepFixToDecDataMap = cataP f g h
  where
    f ann x = M.empty
    g ann x = M.insert (getHash ann) (getConst ann) M.empty
    h ann   = M.insert (getHash ann) (getConst ann)

foldPrepFixToMap :: DecFix kappa fam ix -> M.Map String Int
foldPrepFixToMap = cataP f g h
  where
    f ann x = M.empty
    g ann x = M.insert (getHash ann) (getHeight ann) M.empty
    h ann   = M.insert (getHash ann) (getHeight ann)

foldPrepFixToTrie :: DecFix kappa fam ix -> Tr.Trie Int
foldPrepFixToTrie = cataP hole prim roll
  where
    hole ann x = mempty
    prim ann x = Tr.insert (getHeight ann) (getW64s ann) Tr.empty
    roll ann   = Tr.insert (getHeight ann) (getW64s ann)

convertDecFixToPrepFix :: DecFix kappa fam ix -> PrepFix () kappa fam ix
convertDecFixToPrepFix (Hole' (Const (DecData dig h)) x) = Hole' (Const (PrepData dig h ())) x
convertDecFixToPrepFix (Prim' (Const (DecData dig h)) x) = Prim' (Const (PrepData dig h ())) x
convertDecFixToPrepFix (Roll' (Const (DecData dig h)) x) = Roll' (Const (PrepData dig h ())) (repMap convertDecFixToPrepFix x)

getTestHeight :: DecFix kappa fam x -> Int
getTestHeight (Hole' (Const (DecData _ h)) _) = h
getTestHeight (Prim' (Const (DecData _ h)) _) = h
getTestHeight (Roll' (Const (DecData _ h)) _) = h

decoratePrepFixWithMap :: M.Map String DecData -> DecHashFix kappa fam ix -> DecFix kappa fam ix
decoratePrepFixWithMap m (Hole' (Const (DecHash dig _)) x) = Hole' (Const (DecData dig (-1))) x
decoratePrepFixWithMap m (Prim' (Const (DecHash dig _)) x) = decData
  where
    lookupDecData = M.lookup (show dig) m
    decData = case lookupDecData of
      Nothing -> Prim' (Const (DecData dig 0)) x
      Just dd -> Prim' (Const dd) x
decoratePrepFixWithMap m r@(Roll' (Const (DecHash dig _)) x) = decData
  where
    lookupDecData = M.lookup (show dig) m
    decData = case lookupDecData of
      Nothing -> Roll' (Const (DecData dig (1 + h))) x'
      Just dd -> Roll' (Const dd) x'
    x' = repMap (decoratePrepFixWithMap m) x
    xs = repLeavesList x
    ns = map (exElim (getTestHeight . decoratePrepFixWithMap m)) xs
    h = if not (null ns) then maximum ns else 0
