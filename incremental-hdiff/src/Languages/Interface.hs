{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes            #-}
module Languages.Interface where

import           Data.List                  (elemIndex, splitAt)

import           Control.Monad.Except

import           Generics.Simplistic.Deep
import           Generics.Simplistic.Digest
import           Generics.Simplistic.Util

import           Control.Applicative        ((<|>))
import qualified Languages.While.Parse      as While
import           System.Exit
import           System.IO

type LangCnstr kappa fam ix
  = (All Digestible kappa , All Eq kappa , All Show kappa)

data LangParser :: * where
  LangParser :: (LangCnstr kappa fam ix)
             => String -- ^ Language Extension
             -> (FilePath -> ExceptT String IO (SFix kappa fam ix)) -- ^ Parser
             -> LangParser

data Nat = Z | S Nat
data VectorOf (a :: *) (n :: Nat) :: * where
  V0 :: VectorOf a 'Z
  VS :: a -> VectorOf a n -> VectorOf a ('S n)

redirectErr :: ExceptT String IO a -> IO a
redirectErr f = runExceptT f >>= either myerr return
  where
    myerr str = hPutStrLn stderr str
             >> exitWith (ExitFailure exitFailureParse)
    exitFailureParse = 10

-- |Selects a given parser on a list.
getParserForExt :: String -> [LangParser] -> Maybe LangParser
getParserForExt _   []     = Nothing
getParserForExt queryExt (p@(LangParser parserExt _):ps)
  | parserExt == queryExt = Just p
  | otherwise             = getParserForExt queryExt ps

parserSelect :: (Monad m)
             => Maybe String
             -> [LangParser]
             -> VectorOf FilePath ('S n)
             -> ExceptT String m LangParser
parserSelect ext ps xs = maybe (throwError "No available parser") return
                       $ ((`getParserForExt` ps) =<<)
                       $ ext <|> getSuffix (vHead xs)
  where
    vHead :: VectorOf a ('S n) -> a
    vHead (VS a _) = a

    getSuffix :: String -> Maybe String
    getSuffix str = Just . tail . snd . flip splitAt str =<< elemIndex '.' str

vecMapM :: (Monad m) => (a -> m b) -> VectorOf a n -> m (VectorOf b n)
vecMapM _ V0        = return V0
vecMapM f (VS x xs) = VS <$> f x <*> vecMapM f xs

-- | Given a language parser and a vector of files, attempts
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

withParsedElSel :: Maybe String
                -> [LangParser]
                -> VectorOf FilePath ('S n)
                -> (forall kappa fam ix
                    . (LangCnstr kappa fam ix)
                   => (FilePath -> IO (SFix kappa fam ix))
                   -> VectorOf (SFix kappa fam ix) ('S n)
                   -> IO res)
                -> ExceptT String IO res
withParsedElSel ext parsers fs f = do
  p <- parserSelect ext parsers fs
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
withParsed1 ext parsers file f = redirectErr
                               $ withParsedElSel ext parsers (VS file V0)
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
withParsed2 sel parsers fa fb f = redirectErr
                                $ withParsedElSel sel parsers (VS fa (VS fb V0))
                                $ \p (VS x (VS y V0)) -> f p x y
