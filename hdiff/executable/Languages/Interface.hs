{-# LANGUAGE CPP                    #-}
{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE QuantifiedConstraints  #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE TypeApplications       #-}
module Languages.Interface
  where

import           Data.List                   (elemIndex, splitAt)

import           Control.Monad.Except

import           Generics.Simplistic.Deep
import           Generics.Simplistic.Digest
import           Generics.Simplistic.Util

import           System.Exit
import           System.IO

import qualified Languages.Dyck              as Dyck
import qualified Languages.Lines             as Lines
import qualified Languages.While             as While
#ifdef REAL_LANGUAGES
import qualified Languages.Bash              as Sh
import qualified Languages.Clojure.Interface as Clj
import qualified Languages.Java              as Java
import qualified Languages.JavaScript        as JS
import qualified Languages.Lua               as Lua
import qualified Languages.Python            as Py
#endif

redirectErr :: ExceptT String IO a -> IO a
redirectErr f = runExceptT f >>= either myerr return
  where
    myerr str = hPutStrLn stderr str
             >> exitWith (ExitFailure exitFailureParse)

exitFailureParse :: Int
exitFailureParse = 10

-- |The parsers that we support
mainParsers :: [LangParser]
mainParsers
  = [LangParser "while"    (fmap While.dfromWhile . While.parseFile)
    ,LangParser "lines"    (fmap Lines.dfromLines . Lines.parseFile)
    ,LangParser "dyck"     (fmap Dyck.dfromDyck'  . Dyck.parseFile)
#ifdef REAL_LANGUAGES
    ,LangParser "java"     (fmap Java.dfromJava   . Java.parseFile)
    ,LangParser "lua"      (fmap Lua.dfromLua     . Lua.parseFile)
    ,LangParser "clj"      (fmap Clj.dfromClj     . Clj.parseFile)
    ,LangParser "js"       (fmap JS.dfromJS'      . JS.parseFile)
    ,LangParser "py"       (fmap Py.dfromPy'      . Py.parseFile)
    ,LangParser "sh"       (fmap Sh.dfromSh       . Sh.parseFile)

    -- The *-loc parsers maintian source location informaton
    -- ,LangParser "py-loc"   (fmap Py.dfromPy       . Py.parseFile)
    -- ,LangParser "js-loc"   (fmap JS.dfromJS       . JS.parseFile)
#endif
    ,LangParser "dyck-loc" (fmap Dyck.dfromDyck   . Dyck.parseFile)
    ]

type LangCnstr kappa fam ix
  = (All Digestible kappa , All Eq kappa , All Show kappa)

data LangParser :: * where
  LangParser :: (LangCnstr kappa fams ix)
             -- |Language extension
             => String
             -- |Parser that
             -> (FilePath -> ExceptT String IO (SFix kappa fams ix))
             -> LangParser

parserExtension :: LangParser -> String
parserExtension (LangParser ext _) = ext

-- |Selects a given parser on a list.
getParserForExt :: String -> [LangParser] -> Maybe LangParser
getParserForExt _   []     = Nothing
getParserForExt ext (p:ps)
  | parserExtension p == ext = Just p
  | otherwise                = getParserForExt ext ps

data Nat = Z | S Nat

data VectorOf (a :: *) (n :: Nat) :: * where
  V0 :: VectorOf a 'Z
  VS :: a -> VectorOf a n -> VectorOf a ('S n)

vecMapM :: (Monad m) => (a -> m b) -> VectorOf a n -> m (VectorOf b n)
vecMapM _ V0        = return V0
vecMapM f (VS x xs) = VS <$> f x <*> vecMapM f xs

-- |Given a language parser and a vector of files, attempts
-- to parse these files; if all parses succeed we run the given
-- continuation. If one parser fails the error is returned within
-- the except monad.
withParsedEl :: LangParser
             -> VectorOf FilePath ('S n)
             -> (forall kappa fam ix
                 . (LangCnstr kappa fam ix)
                => (FilePath -> IO (SFix kappa fam ix))
                -> VectorOf (SFix kappa fam ix) ('S n)
                -> IO res)
             -> ExceptT String IO res
withParsedEl (LangParser _ parser) vec f
  = do fs <- vecMapM parser vec
       lift $ f (redirectErr . parser) fs

parserSelect :: (Monad m)
             => Maybe String -> [LangParser] -> VectorOf FilePath ('S n)
             -> ExceptT String m LangParser
parserSelect sel ps xs = maybe (throwError "No available parser") return
                       $ join . fmap (flip getParserForExt ps)
                       $ maybe (getSuffix $ vHead xs) Just sel
  where
    vHead :: VectorOf a ('S n) -> a
    vHead (VS a _) = a

    getSuffix :: String -> Maybe String
    getSuffix str = maybe Nothing (Just . tail . snd . flip splitAt str)
                  $ elemIndex '.' str

withParsedElSel :: Maybe String
                -> [LangParser]
                -> VectorOf FilePath ('S n)
                -> (forall kappa fam ix
                    . (LangCnstr kappa fam ix)
                   => (FilePath -> IO (SFix kappa fam ix))
                   -> VectorOf (SFix kappa fam ix) ('S n)
                   -> IO res)
                -> ExceptT String IO res
withParsedElSel sel parsers fs f = do
  p <- parserSelect sel parsers fs
  withParsedEl p fs f

withParsed1 :: Maybe String
            -> [LangParser]
            -> FilePath
            -> (forall kappa fam ix
                 . (LangCnstr kappa fam ix)
                => (FilePath -> IO (SFix kappa fam ix))
                -> SFix kappa fam ix
                -> IO res)
            -> IO res
withParsed1 sel parsers file f
  = redirectErr
  $ withParsedElSel sel parsers (VS file V0)
  $ \p (VS x V0) -> f p x

withParsed2 :: Maybe String
            -> [LangParser]
            -> FilePath -> FilePath
            -> (forall kappa fam ix
                 . (LangCnstr kappa fam ix)
                => (FilePath -> IO (SFix kappa fam ix))
                -> SFix kappa fam ix
                -> SFix kappa fam ix
                -> IO res)
            -> IO res
withParsed2 sel parsers fa fb f
  = redirectErr
  $ withParsedElSel sel parsers (VS fa (VS fb V0))
  $ \p (VS x (VS y V0)) -> f p x y

withParsed3 :: Maybe String
            -> [LangParser]
            -> FilePath -> FilePath -> FilePath
            -> (forall kappa fam ix
                 . (LangCnstr kappa fam ix)
                => (FilePath -> IO (SFix kappa fam ix))
                -> SFix kappa fam ix
                -> SFix kappa fam ix
                -> SFix kappa fam ix
                -> IO res)
            -> IO res
withParsed3 sel parsers fa fb fc f
  = redirectErr
  $ withParsedElSel sel parsers (VS fa (VS fb (VS fc V0)))
  $ \p (VS x (VS y (VS z V0))) -> f p x y z



