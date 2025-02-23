{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
-- |Illustrates the usage of MRSOP with a custom
--  opaque type universe and the use of HDiff to
--  compute diffs over various languages.
--
module Main
  ( main
  ) where

import           Control.DeepSeq
import qualified Control.Exception        as Exc
import           Control.Monad
import           Options.Applicative
import           System.CPUTime
import           System.Exit
import           System.IO

import           Generics.Simplistic.Deep
import           Generics.Simplistic.Util

import qualified Data.HDiff.Apply         as D
import qualified Data.HDiff.Base          as D
import qualified Data.HDiff.Diff          as D
import qualified Data.HDiff.Diff.Align    as D
import qualified Data.HDiff.Merge         as D

import           HDiff.Options
import           Languages.Interface

time :: (NFData a) => IO a -> IO (Double, a)
time act = do
    t1 <- getCPUTime
    result <- act
    let !res = result `deepseq` result
    t2 <- getCPUTime
    let t :: Double
        t = fromIntegral (t2-t1) * 1e-12
    return (t, res)

main :: IO ()
main = Exc.catch mainBody handler
  where
    handler :: Exc.ErrorCall -> IO ()
    handler err = hPutStrLn stderr ("Error call: " ++ show err)
               >> exitWith (ExitFailure 42)

mainBody :: IO ()
mainBody = execParser hdiffOpts >>= \(verb , pars, opts)
    -> case optionMode opts of
         OptAST   -> mainAST     verb pars opts
         OptDiff  -> mainDiff    verb pars opts
         OptMerge -> mainMerge   verb pars opts
    >>= exitWith

putStrLnErr :: String -> IO ()
putStrLnErr = hPutStrLn stderr

-- * Generic interface

mainAST :: Verbosity -> Maybe String -> Options -> IO ExitCode
mainAST v sel opts = withParsed1 sel mainParsers (optFileA opts)
  $ \_ fa -> do
    unless (v == Quiet) $ print fa
    return ExitSuccess

-- |Applies a patch to an element and either checks it is equal to
--  another element, or returns the result.
tryApply :: (LangCnstr kappa fam ix)
         => Verbosity
         -> D.Patch kappa fam ix
         -> SFix kappa fam ix
         -> Maybe (SFix kappa fam ix)
         -> IO (Maybe (SFix kappa fam ix))
tryApply v patch fa fb
  = case D.patchApply patch fa of
      Nothing -> hPutStrLn stderr "!! apply failed"
              >> when (v == Loud)
                  (hPutStrLn stderr (show fa))
              >> exitWith (ExitFailure 2)
      Just b' -> return $ maybe (Just b') (testEq b') fb
 where
   testEq :: (All Eq kappa)
          => SFix kappa fam ix -> SFix kappa fam ix -> Maybe (SFix kappa fam ix)
   testEq x y = if x == y then Just x else Nothing

-- |Runs our diff algorithm with particular options parsed
-- from the CLI options.
diffWithOpts :: (LangCnstr kappa fam ix)
             => Options
             -> SFix kappa fam ix
             -> SFix kappa fam ix
             -> IO (D.Patch kappa fam ix)
diffWithOpts opts fa fb = do
  let localopts = D.DiffOptions (minHeight opts) (diffMode opts) (globScoped opts)
  return (D.diffOpts localopts fa fb)

mainDiff :: Verbosity -> Maybe String -> Options -> IO ExitCode
mainDiff v sel opts = withParsed2 sel mainParsers (optFileA opts) (optFileB opts)
  $ \_ fa fb -> do
    (secs , patch) <- time (D.align <$> diffWithOpts opts fa fb)
    unless (v == Quiet || withStats opts)
      $ hPutStrLn stdout (show patch)
    when (testApply opts) $ void (tryApply v (holesMap D.disalign patch) fa (Just fb))
    when (withStats opts) $
      putStrLn . unwords $
        [ "time(s):" ++ show secs
        , "n+m:" ++ show (holesSize fa + holesSize fb)
        , "cost:" ++ show (D.patchAlignCost patch)
        ]
    return ExitSuccess

appendConflictsTo :: Maybe FilePath -> D.PatchC kappa fam at -> IO ()
appendConflictsTo Nothing  _ = return ()
appendConflictsTo (Just f) x = do
  let cs = map (exElim D.conflictLbl) $ D.getConflicts x
  withFile f AppendMode (\hd -> mapM_ (hPutStrLn hd) cs)

mainMerge :: Verbosity -> Maybe String -> Options -> IO ExitCode
mainMerge v sel opts = withParsed3 sel mainParsers (optFileA opts) (optFileO opts) (optFileB opts)
  $ \pp fa fo fb -> do
    patchOA <- diffWithOpts opts fo fa
    patchOB <- diffWithOpts opts fo fb
    let momc = D.merge patchOA patchOB
    case momc of
      Nothing  -> return (ExitFailure 13)
      Just omc -> case D.noConflicts omc of
         Nothing -> putStrLnErr " !! Conflicts O->A O->B !!"
                 >> appendConflictsTo (optListConfs opts) omc
                 >> when (v == VeryLoud) (hPutStrLn stdout $ show omc)
                 >> return (ExitFailure 1)
         Just om -> do
           when (v == VeryLoud) (hPutStrLn stdout $ show om)
           mtgt <- sequence (fmap pp (optFileRes opts))
           mres <- tryApply v om fo mtgt
           case mres of
             Just res -> when (v == Loud) (hPutStrLn stdout (show res))
                      >> return ExitSuccess
             Nothing  -> return (ExitFailure 3)
