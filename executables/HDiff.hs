{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE CPP                   #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
-- |Illustrates the usage of MRSOP with a custom
--  opaque type universe and the use of HDiff to
--  compute diffs over various languages.
--
module Main (main) where

import System.IO
import System.Exit
import Control.Monad
import Control.Applicative
import Data.Foldable (asum)

import Options.Applicative
import Data.Semigroup ((<>))

import           Data.Maybe (isJust)
import qualified Data.List as L (lookup)
import           Data.Type.Equality

import Generics.MRSOP.Base hiding (Infix)
import Generics.MRSOP.HDiff.Renderer
import Generics.MRSOP.HDiff.Digest

import qualified Generics.MRSOP.GDiff          as GDiff
import qualified Generics.MRSOP.STDiff         as STDiff

import           Data.Exists
import qualified Data.HDiff.Patch        as D
import qualified Data.HDiff.Diff         as D
import qualified Data.HDiff.Patch.Merge  as D
import qualified Data.HDiff.Patch.TreeEditDistance as TED
import           Data.HDiff.Patch.Show
import qualified Data.HDiff.Change       as D
import qualified Data.HDiff.Change.Merge as D
import qualified Data.HDiff.Change.TreeEditDistance as TEDC

import           Languages.Interface
import           HDiff.Options

main :: IO ()
main = execParser hdiffOpts >>= \(verb , pars, opts)
    -> case optionMode opts of
         OptAST     -> mainAST     verb pars opts
         OptDiff    -> mainDiff    verb pars opts
         OptGDiff   -> mainGDiff   verb pars opts
         OptMerge   -> mainMerge   verb pars opts
         OptSTMerge -> mainSTMerge verb pars opts
    >>= exitWith

putStrLnErr :: String -> IO ()
putStrLnErr = hPutStrLn stderr

-- * Generic interface

mainAST :: Verbosity -> Maybe String -> Options -> IO ExitCode
mainAST v sel opts = withParsed1 sel mainParsers (optFileA opts)
  $ \_ fa -> do
    unless (v == Quiet) $ putStrLn (show (renderFix renderHO fa))
    return ExitSuccess

-- |Applies a patch to an element and either checks it is equal to
--  another element, or returns the result.
tryApply :: (EqHO ki , ShowHO ki , TestEquality ki , IsNat ix, RendererHO ki
            ,HasDatatypeInfo ki fam codes)
         => Verbosity
         -> D.Patch ki codes ix
         -> Fix ki codes ix
         -> Maybe (Fix ki codes ix)
         -> IO (Maybe (Fix ki codes ix))
tryApply v patch fa fb
  = case D.apply patch fa of
      Left err -> hPutStrLn stderr "!! apply failed"
               >> hPutStrLn stderr ("  " ++ err)
               >> when (v == Loud)
                   (hPutStrLn stderr (show $ renderFix renderHO fa))
               >> exitFailure
      Right b' -> return $ maybe (Just b') (const Nothing) fb

-- |Runs our diff algorithm with particular options parsed
-- from the CLI options.
diffWithOpts :: ( EqHO ki , ShowHO ki , TestEquality ki , IsNat ix, RendererHO ki
                , DigestibleHO ki, HasDatatypeInfo ki fam codes)
             => Options
             -> Fix ki codes ix
             -> Fix ki codes ix
             -> IO (D.Patch ki codes ix)
diffWithOpts opts fa fb = do
  let localopts = D.DiffOptions (minHeight opts) (opqHandling opts) (diffMode opts) 
  return (D.diffOpts localopts fa fb)

mainGDiff :: Verbosity -> Maybe String -> Options -> IO ExitCode
mainGDiff _ sel opts = withParsed2 sel mainParsers (optFileA opts) (optFileB opts)
  $ \_ fa fb -> do
    let es = GDiff.diff' fa fb
    putStrLn ("tree-edit-distance: " ++ show (GDiff.cost es))
    when (showES opts) (putStrLn $ show es)
    return ExitSuccess

mainDiff :: Verbosity -> Maybe String -> Options -> IO ExitCode
mainDiff v sel opts = withParsed2 sel mainParsers (optFileA opts) (optFileB opts)
  $ \_ fa fb -> do
    patch <- diffWithOpts opts fa fb
    unless (v == Quiet)   $ displayRawPatch stdout patch
    when (testApply opts) $ void (tryApply v patch fa (Just fb))
    when (isJust (toEditScript opts)) $ do
      let (role , ees) = case toEditScript opts of
                           Just Patch -> ("patch" , TED.toES  patch                  (NA_I fa))
                           Just Chg   -> ("change", TEDC.toES (D.distrCChange patch) (NA_I fa))
      case ees of
        Left  err -> putStrLnErr ("!! " ++ err)
        Right es  -> putStrLn ("tree-edit-distance: " ++ role ++ " " ++ show (GDiff.cost es))
                  >> when (v == Loud) (putStrLn $ show es)
    return ExitSuccess

-- TODO: Implement testing for result
mainMerge :: Verbosity -> Maybe String -> Options -> IO ExitCode
mainMerge v sel opts = withParsed3 sel mainParsers (optFileA opts) (optFileO opts) (optFileB opts)
  $ \pp fa fo fb -> do
    when (v == Loud) $ do
      putStrLnErr $ "O: " ++ optFileO opts
      putStrLnErr $ "A: " ++ optFileA opts
      putStrLnErr $ "B: " ++ optFileB opts
    patchOA <- diffWithOpts opts fo fa
    patchOB <- diffWithOpts opts fo fb
    let res = D.diff3 patchOA patchOB
    when (v == VeryLoud) $ do
      putStrLnErr $ "diff3 O->A O->B " ++ replicate 55 '#'
      displayPatchC stderr res
    case D.noConflicts res of
      Nothing -> putStrLnErr " !! Conflicts O->A O->B !!"
              >> putStrLnErr (unlines
                             $ map (\(Exists (D.Conflict _ _)) -> "conf")
                             $ D.getConflicts res)
              >> return (ExitFailure 1)
      Just om -> do
        when (v == Loud) (putStrLnErr "!! apply om o")
        Just m <- tryApply v om fo Nothing
        when (v == Loud) $ do
          putStrLnErr $ show $ renderFix renderHO m
        return ExitSuccess
          
mainSTMerge :: Verbosity -> Maybe String -> Options -> IO ExitCode
mainSTMerge v sel opts = withParsed3 sel mainParsers (optFileA opts) (optFileO opts) (optFileB opts)
  $ \_ fa fo fb -> do
    when (v == Loud) $ do
      putStrLnErr $ "O: " ++ optFileO opts
      putStrLnErr $ "A: " ++ optFileA opts
      putStrLnErr $ "B: " ++ optFileB opts
    let oa = STDiff.diff fo fa
    let ob = STDiff.diff fo fb
    let resAB = STDiff.merge oa ob
    let resBA = STDiff.merge ob oa
    case (,) <$> resAB <*> resBA of
      Nothing        -> putStrLnErr " !! Conflicts somewhere !!"
                     >> return (ExitFailure 1)
      Just (ab , ba) ->
        case (,) <$> STDiff.apply ab fa <*> STDiff.apply ba fb of
          Nothing        -> putStrLnErr " !! Application Failed !!"
                         >> exitFailure
          Just (fb' , fa') -> return $ if eqHO fb' fa'
                                       then ExitSuccess
                                       else ExitFailure 2
