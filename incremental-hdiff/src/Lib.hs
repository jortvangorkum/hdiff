{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE ExplicitNamespaces  #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lib where

import           CommandLine                (Options (..))

import           Control.Monad.Identity     (Identity (runIdentity))
import           Data.Functor.Const         (Const (..))
import           Data.HDiff                 (DiffOptions)
import           Data.HDiff.Base            (Chg (Chg), Patch)
import           Data.HDiff.Diff.Closure    (close)
import           Data.HDiff.Diff.Types      (DiffOptions (doGlobalChgs, doMinHeight, doMode),
                                             MinHeight, diffOptionsDefault)
import           Data.HDiff.MetaVar         (MetaVar)
import qualified Data.Map                   as M
import           Data.Maybe                 (fromJust)
import           Data.Word                  (Word64)
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
import           WordTrie                   as T

mainAST :: Maybe String -> Options -> IO ExitCode
mainAST ext opts = withParsed1 ext mainParsers (optFileA opts)
  $ \_ fa -> do
    print fa
    return ExitSuccess

decoratePrepFixWithMap :: M.Map String DecData -> DecFix kappa fam ix -> DecFix kappa fam ix
decoratePrepFixWithMap m p@Prim'{} = findOrBuild m p
decoratePrepFixWithMap m r@Roll'{} = findOrBuild m r
decoratePrepFixWithMap m x         = x

mainDiff :: Maybe String -> Options -> IO ExitCode
mainDiff ext opts = withParsed2 ext mainParsers (optFileA opts) (optFileB opts)
  $ \_ fa fb -> do
    let decFa = decorate fa

    let hashFb = decorateHash fb
    -- print hashFb

    let hashMap = foldPrepFixToDecDataMap decFa
    -- print hashMap

    let decFb = decoratePrepFixWithMap hashMap hashFb
    print decFb

    -- let patch = diff 1 decFa decFb
    -- print patch

    return ExitSuccess

mainBody :: Maybe String -> Options -> IO ExitCode
mainBody ext parser = case parser of
  AST{}  -> mainAST ext parser
  Diff{} -> mainDiff ext parser
