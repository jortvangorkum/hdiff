{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module Preprocess where
import           Data.Functor.Const         (Const (..))
import           Data.Proxy                 (Proxy (..))

import           GHC.Generics               (Generic (Rep), V1)
import           Generics.Simplistic        (SRep (..))
import           Generics.Simplistic.Deep
import           Generics.Simplistic.Digest
import qualified Generics.Simplistic.Pretty as D
import           Generics.Simplistic.Util

-- |We precompute the digest of a tree and its height
--  and annotate our fixpoints with this data before
--  going forward and computing a diff.
data PrepData a = PrepData
  { treeDigest :: Digest
  , treeHeight :: Int
  , treeParm   :: a
  } deriving (Eq , Show)

-- |A 'PrepFix' is a prepared fixpoint. In our case, it is
-- just a 'HolesAnn' with the prepared data inside of it.
type PrepFix a kappa fam
  = SFixAnn kappa fam (Const (PrepData a))

-- TODO: Update show to view treedigest and treeheight
instance (Show a, All Show kappa) => Show (PrepFix a kappa fam ix) where
  show = show . D.sfixAnnPretty (const id)





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
         -> PrepFix () kappa fam at
decorate = synthesize (const onRec) (const onPrim) (const botElim)
  where
    botElim :: V1 x -> a
    botElim = error "botElim"

    pp :: Proxy kappa
    pp = Proxy

    onPrim :: (Elem b kappa) => b -> Const (PrepData ()) b
    onPrim b = Const $ PrepData (digPrim pp b) 0 ()

    onRec :: SRep (Const (PrepData ())) (Rep b)
          -> Const (PrepData ()) b
    onRec sr = let dig = authAlg (treeDigest . getConst) sr
                   h   = 1 + maxAlg (treeHeight . getConst) sr
                in Const $ PrepData dig h ()
