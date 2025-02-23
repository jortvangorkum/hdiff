{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# OPTIONS_GHC -Wno-orphans                            #-}
{-# OPTIONS_GHC -Wno-missing-signatures                 #-}
{-# OPTIONS_GHC -Wno-missing-pattern-synonym-signatures #-}
module Languages.Lua
  where

#ifdef REAL_LANGUAGES

import qualified Language.Lua.Parser         as Lua
import           Language.Lua.Syntax

import           Control.Monad.Except
import           Data.Text                   (Text)

import           Generics.Simplistic
import           Generics.Simplistic.Deep
import           Generics.Simplistic.Deep.TH

type LuaPrim = '[ Text , Bool ]
type LuaFam =
  [ Block
  , (Maybe Block)
  , [Stat]
  , Stat
  , [(Exp , Block)]
  , (Exp , Block)
  , Exp
  , (Maybe Exp)
  , [TableField]
  , TableField
  , Name
  , NumberType
  , FunBody
  , [Name]
  , PrefixExp
  , Var
  , FunCall
  , FunArg
  , FunName
  , (Maybe Name)
  , [Exp]
  , (Maybe [Exp])
  , Binop
  , Unop
  , [Var]
  ]

deriveDeepFor ''LuaPrim ''LuaFam

-- instance Deep LuaPrim LuaFam Block
-- instance Deep LuaPrim LuaFam (Maybe Block)
-- instance Deep LuaPrim LuaFam [Stat]
-- instance Deep LuaPrim LuaFam Stat
-- instance Deep LuaPrim LuaFam [(Exp , Block)]
-- instance Deep LuaPrim LuaFam (Exp , Block)
-- instance Deep LuaPrim LuaFam Exp
-- instance Deep LuaPrim LuaFam (Maybe Exp)
-- instance Deep LuaPrim LuaFam [TableField]
-- instance Deep LuaPrim LuaFam TableField
-- instance Deep LuaPrim LuaFam Name
-- instance Deep LuaPrim LuaFam NumberType
-- instance Deep LuaPrim LuaFam FunBody
-- instance Deep LuaPrim LuaFam [Name]
-- instance Deep LuaPrim LuaFam PrefixExp
-- instance Deep LuaPrim LuaFam Var
-- instance Deep LuaPrim LuaFam FunCall
-- instance Deep LuaPrim LuaFam FunArg
-- instance Deep LuaPrim LuaFam FunName
-- instance Deep LuaPrim LuaFam (Maybe Name)
-- instance Deep LuaPrim LuaFam [Exp]
-- instance Deep LuaPrim LuaFam (Maybe [Exp])
-- instance Deep LuaPrim LuaFam Binop
-- instance Deep LuaPrim LuaFam Unop
-- instance Deep LuaPrim LuaFam [Var]



parseFile :: String -> ExceptT String IO Block
parseFile file = do
  res <- lift $ Lua.parseFile file
  case res of
    Left e  -> throwError (show e)
    Right r -> return r

dfromLua :: Block -> SFix LuaPrim LuaFam Block
dfromLua = dfrom

#endif
