{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Generics.Simplistic.Zipper where

import Data.Proxy
import Data.Type.Equality
import GHC.Generics
import Data.Functor.Sum
import Control.Monad.State
import Control.Arrow (first)

import qualified Data.Set as S

import Generics.Simplistic
import Generics.Simplistic.Util

import Debug.Trace

data SZip ty w f where
  Z_L1    ::                SZip ty w f -> SZip ty w (f :+: g)
  Z_R1    ::                SZip ty w g -> SZip ty w (f :+: g)
  Z_PairL :: SZip ty w f -> SRep    w g -> SZip ty w (f :*: g)
  Z_PairR :: SRep   w f  -> SZip ty w g -> SZip ty w (f :*: g)
  Z_M1    :: SMeta i t   -> SZip ty w f -> SZip ty w (M1 i t f)
  Z_KH     :: a :~: ty   -> SZip ty w (K1 i a)
deriving instance (forall a. Show (w a)) => Show (SZip h w f)

zipperMap :: (forall x . h x -> g x)
          -> SZip ty h f -> SZip ty g f
zipperMap f (Z_L1 x) = Z_L1 (zipperMap f x)
zipperMap f (Z_R1 x) = Z_R1 (zipperMap f x)
zipperMap f (Z_M1 c x) = Z_M1 c (zipperMap f x)
zipperMap f (Z_PairL x y) = Z_PairL (zipperMap f x) (repMap f y)
zipperMap f (Z_PairR x y) = Z_PairR (repMap f x) (zipperMap f y)
zipperMap f (Z_KH x) = Z_KH x

data Zipper f g t where
  Zipper :: { zipper :: SZip t f (Rep t)
            , plug   :: g t
            }
         -> Zipper f g t

type Zipper' fam prim ann phi
  = Zipper (HolesAnn fam prim ann phi)
           (HolesAnn fam prim ann phi)

zippers :: forall fam prim ann phi t
         . (HasDecEq fam)
        => (forall a . (Elem t fam) => phi a -> Maybe (a :~: t)) 
        -> HolesAnn fam prim ann phi t
        -> [Zipper' fam prim ann phi t] 
zippers _   (Prim' _ _) = []
zippers _   (Hole' _ _) = []
zippers aux (Roll' _ r) = map (uncurry Zipper) (go r)
  where
    pf :: Proxy fam
    pf = Proxy

    pa :: HolesAnn fam prim ann phi a -> Proxy a
    pa _ = Proxy

    go :: SRep (HolesAnn fam prim ann phi) f
       -> [(SZip t (HolesAnn fam prim ann phi) f
          , HolesAnn fam prim ann phi t)]
    go S_U1       = []
    go (S_L1 x)   = first Z_L1 <$> go x
    go (S_R1 x)   = first Z_R1 <$> go x
    go (S_M1 c x) = first (Z_M1 c) <$> go x
    go (x :**: y) = (first (flip Z_PairL y) <$> go x)
                 ++ (first (Z_PairR x)      <$> go y)
    go (S_K1 x@(Roll' _ _)) =
      case sameTy pf (Proxy :: Proxy t) (pa x) of
        Just Refl -> return $ (Z_KH Refl , x)
        Nothing   -> []
    go (S_K1 x@(Hole' _ xh)) = 
      case aux xh of
        Just Refl -> return $ (Z_KH Refl , x)
        Nothing   -> []
    go _ = []
      
      
-------------------
-- Prtty

zipConstructorName :: SZip h w f -> String
zipConstructorName (Z_M1 x@SM_C _)
  = getConstructorName x
zipConstructorName (Z_M1 _ x)
  = zipConstructorName x
zipConstructorName (Z_L1 x)
  = zipConstructorName x
zipConstructorName (Z_R1 x)
  = zipConstructorName x
zipConstructorName _
  = error "Please; use GHC's deriving mechanism. This keeps M1's at the top of the Rep"

zipLeavesList :: SZip ty w f -> [Maybe (Exists w)]
zipLeavesList (Z_L1 x) = zipLeavesList x
zipLeavesList (Z_R1 x) = zipLeavesList x
zipLeavesList (Z_M1 _ x) = zipLeavesList x
zipLeavesList (Z_PairL l x) = zipLeavesList l ++ map Just (repLeavesList x)
zipLeavesList (Z_PairR x l) = map Just (repLeavesList x) ++ zipLeavesList l
zipLeavesList (Z_KH _)      = [Nothing]
