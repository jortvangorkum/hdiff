{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE UndecidableInstances #-}

module Types where

import           Data.Functor.Const                        (Const (..))
import           Data.Proxy
import qualified Data.Text                                 as T
import           Data.Text.Prettyprint.Doc
import           Data.Text.Prettyprint.Doc.Render.Terminal
import           Generics.Simplistic
import           Generics.Simplistic.Deep
import           Generics.Simplistic.Digest
import           Generics.Simplistic.Util

data DecHash = DecHash { dig :: Digest, height :: Maybe Int }
  deriving (Eq, Show)

data DecData = DecData
  { treeDigest :: Digest
  , treeHeight :: Int
  } deriving (Eq, Show)

type DecFix kappa fam = SFixAnn kappa fam (Const DecData)
type DecHashFix kappa fam = SFixAnn kappa fam (Const DecHash)

instance (All Show kappa) => Show (DecFix kappa fam ix) where
  show = myRender . decFixPretty

myRender :: Doc AnsiStyle -> String
myRender =
  let maxWidth = 80
      pgdim = LayoutOptions (AvailablePerLine maxWidth 1)
      layout = layoutSmart pgdim
   in T.unpack . renderStrict . layout

annPretty :: String -> Const DecData ix -> Doc AnsiStyle
annPretty t (Const (DecData treeDigest treeHeight)) = annotate (style <> bold) node
  where
    node = list [hash, height]
    height = pretty $ show treeHeight
    hash = pretty $ take 5 (show (getDigest treeDigest))
    style = case t of
      "hole" -> color Red
      "prim" -> color Blue
      "roll" -> color Yellow
      _      -> color White

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

decFixPretty :: forall kappa fam ix . (All Show kappa)
             => DecFix kappa fam ix
             -> Doc AnsiStyle
decFixPretty (Hole' ann x) = annPretty "hole" ann <+> annotate (color Green) (pretty "[]")
decFixPretty (Prim' ann x) = annPretty "prim" ann <+> pretty (wshow (Proxy :: Proxy kappa) x)
decFixPretty (Roll' ann x) = annPretty "roll" ann <+> repPretty decFixPretty x
