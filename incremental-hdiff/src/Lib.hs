{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds     #-}
module Lib where

import           CommandLine                (Options (..))

import           Data.Functor.Const         (Const (..))
import qualified Data.Map                   as M
import           Data.Maybe                 (fromJust)
import           Data.Word                  (Word64)
import           Generics.Simplistic        (repLeavesList)
import           Generics.Simplistic.Deep
import           Generics.Simplistic.Digest (Digest (getDigest))
import qualified Generics.Simplistic.Digest as D
import           Generics.Simplistic.Util   (exElim)
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

foldToMap :: M.Map String ix -> PrepFix a kappa fam ix -> M.Map String ix
foldToMap m (Hole' _ _) = m
foldToMap m (Prim' ann x) = M.insert k x m
  where
    (Const (PrepData treeDigest _ treeVal)) = ann
    k = show $ getDigest treeDigest
foldToMap m (Roll' ann x) = ms
  where
    -- TODO: Trying to find out where the value of the node is stored and put that in the Map as value
    ms = M.unions $ map (exElim (foldToMap m)) xs
    (Const (PrepData treeDigest _ treeVal)) = ann
    k = show $ getDigest treeDigest
    xs = repLeavesList x

mainDiff :: Maybe String -> Options -> IO ExitCode
mainDiff ext opts = withParsed2 ext mainParsers (optFileA opts) (optFileB opts)
  $ \_ fa fb -> do
    let decFa = decorate fa
    print decFa

    let treeMap = foldToMap M.empty decFa
    print treeMap

    return ExitSuccess

mainBody :: Maybe String -> Options -> IO ExitCode
mainBody ext parser = case parser of
  AST{}  -> mainAST ext parser
  Diff{} -> mainDiff ext parser
