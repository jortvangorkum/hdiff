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

getTestHeight :: DecFix kappa fam x -> Int
getTestHeight (Hole' (Const (DecData _ h)) _) = h
getTestHeight (Prim' (Const (DecData _ h)) _) = h
getTestHeight (Roll' (Const (DecData _ h)) _) = h

decoratePrepFixWithMap :: M.Map String DecData -> DecHashFix kappa fam ix -> DecFix kappa fam ix
decoratePrepFixWithMap m (Hole' (Const (DecHash dig _)) x) = Hole' (Const (DecData dig (-1))) x
decoratePrepFixWithMap m (Prim' (Const (DecHash dig _)) x) = decData
  where
    lookupDecData = M.lookup (show dig) m
    decData = case lookupDecData of
      Nothing -> Prim' (Const (DecData dig 0)) x
      Just dd -> Prim' (Const dd) x
decoratePrepFixWithMap m r@(Roll' (Const (DecHash dig _)) x) = decData
  where
    lookupDecData = M.lookup (show dig) m
    decData = case lookupDecData of
      Nothing -> Roll' (Const (DecData dig (1 + h))) x'
      Just dd -> Roll' (Const dd) x'
    x' = repMap (decoratePrepFixWithMap m) x
    xs = repLeavesList x
    ns = map (exElim (getTestHeight . decoratePrepFixWithMap m)) xs
    h = if not (null ns) then maximum ns else 0

testIncremental :: (All Digestible kappa1, All Digestible kappa2, Monad m)
                => SFix kappa1 fam1 ix1
                -> SFix kappa2 fam2 ix2
                -> m ( DecFix kappa1 fam1 ix1
                     , DecFix kappa2 fam2 ix2)
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
    -- (!decFa, !decFb) <- testIncremental fa fb
    (!decFa, !decFb) <- testOriginal fa fb

    -- let prepFa = convertDecFixToPrepFix decFa
    -- let prepFb = convertDecFixToPrepFix decFb

    -- let patch = diff 1 prepFa prepFb
    -- print patch

    return ExitSuccess

mainBody :: Maybe String -> Options -> IO ExitCode
mainBody ext parser = case parser of
  AST{}  -> mainAST ext parser
  Diff{} -> mainDiff ext parser
