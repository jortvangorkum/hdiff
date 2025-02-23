{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
module Data.HDiff.Diff.Modes where

import           Data.Functor.Const
import qualified Data.Set                   as S

import           GHC.Generics
import           Generics.Simplistic
import           Generics.Simplistic.Deep
import           Generics.Simplistic.Digest
import           Generics.Simplistic.Util

import           Data.HDiff.Diff.Preprocess
import           Data.HDiff.Diff.Types
import           Data.HDiff.MetaVar
import qualified Data.WordTrie              as T


-- |A predicate indicating whether a tree can be shared.
type CanShare kappa fam = forall a ix . PrepFix a kappa fam ix -> Bool

extractHoles :: DiffMode
             -> CanShare kappa fam
             -> IsSharedMap
             -> Delta (PrepFix a kappa fam) at
             -> Delta (Holes kappa fam (MetaVar kappa fam)) at
extractHoles DM_NoNested h tr sd
  = extractNoNested h tr sd
extractHoles DM_ProperShare h tr (src :*: dst)
  = (extractProperShare h tr src :*: extractProperShare h tr dst)
extractHoles DM_Patience h tr (src :*: dst)
  = (extractPatience h tr src :*: extractPatience h tr dst)

-- ** Proper Shares

extractProperShare :: CanShare kappa fam
                   -> IsSharedMap
                   -> PrepFix a kappa fam at
                   -> Holes kappa fam (MetaVar kappa fam) at
extractProperShare h tr a = properShare h tr (tagProperShare tr a)

tagProperShare :: forall a kappa fam at
                . IsSharedMap
               -> PrepFix a kappa fam at
               -> PrepFix (Int , Bool) kappa fam at
tagProperShare ism = synthesize onRec onPrim (const botElim)
  where
    botElim :: V1 x -> y
    botElim = error "impossible"

    myar :: PrepData x -> Int
    myar = maybe 0 getArity . flip T.lookup ism . toW64s . treeDigest

    onPrim :: (Elem b kappa)
           => Const (PrepData a) b
           -> b
           -> Const (PrepData (Int , Bool)) b
    onPrim (Const pd) _ = Const $ pd { treeParm = (myar pd , True) }

    onRec :: Const (PrepData a) b
          -> SRep (Const (PrepData (Int, Bool))) (Rep b)
          -> Const (PrepData (Int, Bool)) b
    onRec (Const pd) p
      = let maxar = maxAlg (fst . treeParm . getConst) p
            myar' = myar pd
         in Const $ pd { treeParm = (max maxar myar' , myar' >= maxar) }

properShare :: forall kappa fam at
             . CanShare kappa fam
            -> IsSharedMap
            -> PrepFix (Int , Bool) kappa fam at
            -> Holes kappa fam (MetaVar kappa fam) at
properShare _ _ (PrimAnn _ k) = Prim k
properShare h tr pr@(SFixAnn ann d)
  = let prep  = getConst ann
        isPS  = snd $ treeParm prep
     in if not (isPS && h pr)
        then Roll (repMap (properShare h tr) d)
        else case T.lookup (toW64s $ treeDigest prep) tr of
               Nothing -> Roll (repMap (properShare h tr) d)
               Just i  -> Hole (MV_Comp $ getMetavar i)

-- ** Patience

extractPatience :: CanShare kappa fam
                -> IsSharedMap
                -> PrepFix a kappa fam at
                -> Holes kappa fam (MetaVar kappa fam) at
extractPatience h tr a = patience h tr a

patience :: forall kappa fam at a
          . CanShare kappa fam
         -> IsSharedMap
         -> PrepFix a kappa fam at
         -> Holes kappa fam (MetaVar kappa fam) at
patience _ _ (PrimAnn _ k) = Prim k
patience h tr pr@(SFixAnn ann d)
  = let aux = Roll (repMap (patience h tr) d)
     in if not (h pr)
        then aux
        else case T.lookup (toW64s $ treeDigest $ getConst ann) tr of
               Just i | getArity i == 2 -> Hole (MV_Comp $ getMetavar i)
                      | otherwise       -> aux
               Nothing                  -> aux


-- ** No Nested

extractNoNested :: CanShare kappa fam
                -> IsSharedMap
                -> Delta (PrepFix a kappa fam) at
                -> Delta (Holes kappa fam (MetaVar kappa fam)) at
extractNoNested h tr (src :*: dst)
  = let del'  = noNested h tr src
        ins'  = noNested h tr dst
        delHs = S.fromList $ map getHole $ holesHolesList del'
        insHs = S.fromList $ map getHole $ holesHolesList ins'
        holes = delHs `S.intersection` insHs
        del   = holesRefineHoles (refineHole holes) del'
        ins   = holesRefineHoles (refineHole holes) ins'
     in (del :*: ins)
  where
    getHole :: Exists (Const Int :*: f) -> Int
    getHole (Exists (Const v :*: _)) = v

    refineHole :: S.Set Int
               -> (Const Int :*: PrepFix a kappa fam) ix
               -> Holes kappa fam (MetaVar kappa fam) ix
    refineHole s (Const i :*: f)
      | i `S.member` s = case f of
                           (SFixAnn _ _) -> Hole (MV_Comp i)
                           (PrimAnn _ _) -> Hole (MV_Prim i)
      | otherwise      = holesMapAnn (error "imp: void") (const U1) f

noNested :: forall kappa fam at a
          . CanShare kappa fam
         -> IsSharedMap
         -> PrepFix a kappa fam at
         -> Holes kappa fam (Const Int :*: PrepFix a kappa fam) at
noNested _ _ (PrimAnn _ x) = Prim x
noNested h tr pr@(SFixAnn ann d)
  = if not (h pr)
    then Roll (repMap (noNested h tr) d)
    else case T.lookup (toW64s $ treeDigest $ getConst ann) tr of
           Nothing -> Roll (repMap (noNested h tr) d)
           Just i  -> Hole (Const (getMetavar i) :*: pr)
