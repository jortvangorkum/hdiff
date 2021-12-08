{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE ExplicitNamespaces  #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lib where

import           CommandLine                (Options (..))

import           Control.Monad.Identity     (Identity (runIdentity))
import           Data.Functor.Const         (Const (..))
import           Data.HDiff.Base            (Chg (Chg), Patch)
import           Data.HDiff.Diff.Closure    (close)
import           Data.HDiff.Diff.Types      (DiffOptions (doGlobalChgs, doMinHeight, doMode),
                                             MinHeight, diffOptionsDefault)
import           Data.HDiff.MetaVar         (MetaVar)
import qualified Data.Map                   as M
import           Data.Maybe                 (fromJust)
import           Data.Word                  (Word64)
import           Diff                       (diff)
import           Generics.Simplistic        (Generic (Rep), SRep, V1 (..),
                                             repLeaves, repLeavesList, repMap,
                                             repMapM, toS, type (:*:) ((:*:)))
import           Generics.Simplistic.Deep
import           Generics.Simplistic.Digest (Digest (getDigest), Digestible,
                                             toW64s)
import qualified Generics.Simplistic.Digest as D
import           Generics.Simplistic.Util   (All, Delta, Elem, Exists (Exists),
                                             exElim, (:*:))
import           Languages.Interface
import           Languages.Main
import           Options.Applicative        (execParser)
import           Preprocess
import           System.Exit
import           Types

mainAST :: Maybe String -> Options -> IO ExitCode
mainAST ext opts = withParsed1 ext mainParsers (optFileA opts)
  $ \_ fa -> do
    print fa
    return ExitSuccess

testIncremental :: (All Digestible kappa1, All Digestible kappa2, Monad m)
                => SFix kappa1 fam1 ix1
                -> SFix kappa2 fam2 ix2
                -> m (DecFix kappa1 fam1 ix1,
                      DecFix kappa2 fam2 ix2)
testIncremental fa fb = do
    let decFa = decorate fa

    let hashFb = decorateHash fb
    -- print hashFb

    let hashMap = foldPrepFixToDecDataMap decFa
    -- print hashMap

    let decFb = decoratePrepFixWithMap hashMap hashFb

    return (decFa, decFb)

testOriginal :: (All Digestible kappa1, All Digestible kappa2, Monad m)
                => SFix kappa1 fam1 ix1
                -> SFix kappa2 fam2 ix2
                -> m ( DecFix kappa1 fam1 ix1
                     , DecFix kappa2 fam2 ix2)
testOriginal fa fb = do
    let decFa = decorate fa
    let decFb = decorate fb

    return (decFa, decFb)

mainDiff :: Maybe String -> Options -> IO ExitCode
mainDiff ext opts = withParsed2 ext mainParsers (optFileA opts) (optFileB opts)
  $ \_ fa fb -> do
    (!decFa, !decFb) <- testIncremental fa fb
    -- (!decFa, !decFb) <- testOriginal fa fb

    -- let prepFa = convertDecFixToPrepFix decFa
    -- let prepFb = convertDecFixToPrepFix decFb

    -- let patch = diff 1 prepFa prepFb
    -- print patch

    return ExitSuccess

mainBody :: Maybe String -> Options -> IO ExitCode
mainBody ext parser = case parser of
  AST{}  -> mainAST ext parser
  Diff{} -> mainDiff ext parser
