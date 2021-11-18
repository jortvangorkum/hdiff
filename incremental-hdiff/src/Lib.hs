module Lib where

import           CommandLine           (Options (..))
import           Data.HDiff            (patchApply)
import qualified Data.HDiff            as D
import qualified Data.HDiff.Diff.Align as D
import           Data.HDiff.Diff.Types (DiffOptions (doMinHeight),
                                        diffOptionsDefault)
import           Data.HDiff.Show
import qualified Data.Map              as M
import           Data.Maybe            (fromJust)
import           Data.Word             (Word64)
import           Languages.Interface
import           Languages.Main
import           Options.Applicative   (execParser)
import           Preprocess            (decorate)
import           System.Exit

mainAST :: Maybe String -> Options -> IO ExitCode
mainAST ext opts = withParsed1 ext mainParsers (optFileA opts)
  $ \_ fa -> do
    print fa
    return ExitSuccess

mainDiff :: Maybe String -> Options -> IO ExitCode
mainDiff ext opts = withParsed2 ext mainParsers (optFileA opts) (optFileB opts)
  $ \_ fa fb -> do
    let decFa = decorate fa
    print decFa

    return ExitSuccess

mainBody :: Maybe String -> Options -> IO ExitCode
mainBody ext parser = case parser of
  AST{}  -> mainAST ext parser
  Diff{} -> mainDiff ext parser
