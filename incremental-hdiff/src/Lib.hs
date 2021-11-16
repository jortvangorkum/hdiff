{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Lib where

import           CommandLine
import qualified Data.HDiff               as D
import qualified Data.HDiff.Diff.Align    as D
import           Data.HDiff.Show
import           Generics.Simplistic.Deep (SFix)
import           Languages.Interface
import           Languages.Main
import           Options.Applicative      (execParser)
import           System.Exit


mainAST :: Maybe String -> Options -> IO ExitCode
mainAST ext opts = withParsed1 ext mainParsers (optFileA opts)
  $ \_ fa -> do
    print fa
    return ExitSuccess

-- |Runs our diff algorithm with particular options parsed
-- from the CLI options.
diffWithOpts :: (LangCnstr kappa fam ix)
             => Options
             -> SFix kappa fam ix
             -> SFix kappa fam ix
             -> IO (D.Patch kappa fam ix)
diffWithOpts opts fa fb = do
  let localopts = D.DiffOptions 1 D.DM_NoNested False
  return (D.diffOpts localopts fa fb)

mainDiff :: Maybe String -> Options -> IO ExitCode
mainDiff ext opts = withParsed2 ext mainParsers (optFileA opts) (optFileB opts)
  $ \_ fa fb -> do
    patch <- D.align <$> diffWithOpts opts fa fb
    print patch
    return ExitSuccess

mainBody :: Maybe String -> Options -> IO ExitCode
mainBody ext parser = case parser of
  AST{}  -> mainAST ext parser
  Diff{} -> mainDiff ext parser
