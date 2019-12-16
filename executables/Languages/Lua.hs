{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE CPP                   #-}
{-# OPTIONS_GHC -Wno-orphans                            #-}
{-# OPTIONS_GHC -Wno-missing-signatures                 #-}
{-# OPTIONS_GHC -Wno-missing-pattern-synonym-signatures #-}
module Languages.Lua where

import Language.Lua.Syntax
import qualified Language.Lua.Parser as Lua

import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.Type.Equality

import           Data.Text.Prettyprint.Doc hiding (braces,parens,semi)
import qualified Data.Text as T

import Control.Monad.Except

import Generics.MRSOP.TH
import Generics.MRSOP.Base

import Generics.MRSOP.HDiff.Digest
import Generics.MRSOP.HDiff.Renderer

data LuaKon = LuaText | LuaBool
data LuaSingl (kon :: LuaKon) :: * where
  SLuaText :: Text -> LuaSingl 'LuaText
  SLuaBool :: Bool -> LuaSingl 'LuaBool

instance RendererHO LuaSingl where
  renderHO (SLuaText t) = pretty (T.unpack t)
  renderHO (SLuaBool b) = pretty b

instance Digestible Text where
  digest = hash . encodeUtf8

instance DigestibleHO LuaSingl where
  digestHO (SLuaText text) = hash (encodeUtf8 text)
  digestHO (SLuaBool bool) = hashStr (show bool)

deriving instance Show (LuaSingl k)
deriving instance Eq (LuaSingl k)

instance EqHO LuaSingl where
  eqHO = (==)

instance ShowHO LuaSingl where
  showHO = show


instance TestEquality LuaSingl where
  testEquality (SLuaText _) (SLuaText _) = Just Refl
  testEquality (SLuaBool _) (SLuaBool _) = Just Refl
  testEquality _ _ = Nothing

deriveFamilyWith ''LuaSingl [t| Block |]

parseFile :: String -> ExceptT String IO Block
parseFile file = do
  res <- lift $ Lua.parseFile file
  case res of
    Left e  -> throwError (show e) 
    Right r -> return r

type W = LuaSingl
type Stmt = Block
type FamStmt = FamBlock
type CodesStmt = CodesBlock
