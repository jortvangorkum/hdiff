{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE RankNTypes          #-}
module Lib where

import           CommandLine                (Options (..))

import           Data.Functor.Const         (Const (..))
import qualified Data.Map                   as M
import           Data.Maybe                 (fromJust)
import           Data.Word                  (Word64)
import           Generics.Simplistic        (SRep, repLeavesList)
import           Generics.Simplistic.Deep
import           Generics.Simplistic.Digest (Digest (getDigest))
import qualified Generics.Simplistic.Digest as D
import           Generics.Simplistic.Util   (Exists (Exists), exElim)
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

-- foldToMap :: M.Map String (PrepFix a kappa fam ix)
--           -> PrepFix a kappa fam ix
--           -> M.Map String (PrepFix a kappa fam ix)
-- foldToMap m h@(Hole' ann _) = M.insert k h m
--   where
--     k = getHash ann
-- foldToMap m p@(Prim' ann x) = M.insert k p m
--   where
--     k = getHash ann
-- foldToMap m r@(Roll' ann x) = M.insert k r m <> ms
--   where
--     -- TODO: Trying to find out where the value of the node is stored and put that in the Map as value
--     ms = M.unions $ map f xs
--     f = exElim (foldToMap m)
--     k = getHash ann
--     xs = repLeavesList x

mainDiff :: Maybe String -> Options -> IO ExitCode
mainDiff ext opts = withParsed2 ext mainParsers (optFileA opts) (optFileB opts)
  $ \_ fa fb -> do
    let decFa = decorate fa
    print decFa

    let hashes = testFold decFa
    print hashes

    return ExitSuccess

mainBody :: Maybe String -> Options -> IO ExitCode
mainBody ext parser = case parser of
  AST{}  -> mainAST ext parser
  Diff{} -> mainDiff ext parser
