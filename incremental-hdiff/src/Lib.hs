{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Lib where

import           CommandLine                (Options (..))

import           Data.Functor.Const         (Const (..))
import qualified Data.Map                   as M
import           Data.Maybe                 (fromJust)
import           Data.Word                  (Word64)
import           Generics.Simplistic        (Generic (Rep), SRep, V1, repLeaves,
                                             repLeavesList, toS)
import           Generics.Simplistic.Deep
import           Generics.Simplistic.Digest (Digest (getDigest))
import qualified Generics.Simplistic.Digest as D
import           Generics.Simplistic.Util   (All, Exists (Exists), exElim)
import           Languages.Interface
import           Languages.Main
import           Options.Applicative        (execParser)
import           Preprocess                 (PrepData (..), PrepFix, decorate)
import           System.Exit

mainAST :: Maybe String -> Options -> IO ExitCode
mainAST ext opts = withParsed1 ext mainParsers (optFileA opts)
  $ \_ fa -> do
    print fa
    return ExitSuccess

holesFold :: (Monoid m, Foldable f)
          => (forall x a . ann a -> x -> m)
          -> (forall a   . ann a -> m -> m)
          -> HolesAnn kappa fam ann f a
          -> m
holesFold f _ (Hole' ann x) = foldMap (f ann) x
holesFold f _ (Prim' ann x) = f ann x
holesFold f g (Roll' ann x) = g ann ms
  where
    xs = repLeavesList x
    ms = mconcat $ map (exElim (holesFold f g)) xs


testFold :: PrepFix a kappa fam ix -> [String]
testFold = holesFold f g
  where
    f ann x = [take 5 (getHash ann)]
    g ann m = [take 5 (getHash ann)] <> m

getHash :: Const (PrepData a) ix -> String
getHash (Const (PrepData treeDigest _ _)) = show $ getDigest treeDigest

{-
  The problem is that because of SRep expecting a (* -> *) type.
  PrepFix loses the type ix, which is then needed in the result of the Map.

  So we need to find a way to get type ix out of SRep
-}
repList :: forall a kappa fam ix . SRep (PrepFix a kappa fam) (Rep ix) -> [PrepFix a kappa fam ix]
repList s = ss -- repLeaves ((:[]) . Exists) (++) []
  where
    f :: PrepFix a kappa fam x -> [PrepFix a kappa fam ix]
    f x = [x]
    g :: [PrepFix a kappa fam ix] -> [PrepFix a kappa fam ix] -> [PrepFix a kappa fam ix]
    g = (++)
    ss :: [PrepFix a kappa fam ix]
    ss = repLeaves f g [] s

foldToMap :: forall kappa fam ix . PrepFix () kappa fam ix -> M.Map String (PrepFix () kappa fam ix)
foldToMap h@(Hole' ann x) = M.insert (getHash ann) h M.empty
foldToMap p@(Prim' ann x) = M.insert (getHash ann) p M.empty
foldToMap r@(Roll' ann x) = M.insert (getHash ann) r m
  where
    m = mconcat $ foldToMap <$> xs
    xs = repList x

mainDiff :: Maybe String -> Options -> IO ExitCode
mainDiff ext opts = withParsed2 ext mainParsers (optFileA opts) (optFileB opts)
  $ \_ fa fb -> do
    let decFa = decorate fa
    print decFa

    let hashMap = foldToMap decFa
    print hashMap

    return ExitSuccess

mainBody :: Maybe String -> Options -> IO ExitCode
mainBody ext parser = case parser of
  AST{}  -> mainAST ext parser
  Diff{} -> mainDiff ext parser
