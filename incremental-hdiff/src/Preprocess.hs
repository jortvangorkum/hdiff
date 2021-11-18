{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}

{-# LANGUAGE UndecidableInstances #-}
module Preprocess where
import           Data.Functor.Const                        (Const (..))
import           Data.Proxy                                (Proxy (..))
import qualified Data.Text                                 as T
import           Data.Text.Prettyprint.Doc
import           Data.Text.Prettyprint.Doc.Render.Terminal
import           GHC.Generics                              (Generic (Rep), V1)
import           Generics.Simplistic
import           Generics.Simplistic.Deep
import           Generics.Simplistic.Digest
import qualified Generics.Simplistic.Pretty                as D
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

myRender :: Doc AnsiStyle -> String
myRender =
  let maxWidth = 80
      pgdim = LayoutOptions (AvailablePerLine maxWidth 1)
      layout = layoutSmart pgdim
   in T.unpack . renderStrict . layout

repPretty :: (forall x . phi x -> Doc ann)
          -> SRep phi f -> Doc ann
repPretty f x =
  group $ parens
        $ nest 1
        $ sep
        $ (pretty c:)
        $ map (exElim f) xs
  where
    c = repConstructorName x
    xs = repLeavesList x
    isParens = if null xs then id else parens

annPretty :: Const (PrepData x) ix -> Doc AnsiStyle
annPretty (Const (PrepData treeDigest treeHeight treeParm)) = annotate style node
  where
    node = list [hash, height]
    height = pretty $ show treeHeight
    hash = pretty $ take 5 (show (getDigest treeDigest))
    style = color Yellow <> bold

prepFixPretty :: forall kappa fam phi h ann a ix
   . (All Show kappa)
  => PrepFix a kappa fam ix
  -> Doc AnsiStyle
prepFixPretty (Hole' ann x) = pretty $ show x
prepFixPretty (Prim' ann x) = pretty $ wshow (Proxy :: Proxy kappa) x
prepFixPretty (Roll' ann x) = annPretty ann <+> repPretty prepFixPretty x

instance (Show a, All Show kappa) => Show (PrepFix a kappa fam ix) where
  show = myRender . prepFixPretty

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
