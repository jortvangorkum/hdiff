{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# OPTIONS_GHC -Wno-missing-signatures                 #-}
{-# OPTIONS_GHC -Wno-missing-pattern-synonym-signatures #-}
module Languages.While.Parse
  where

import           Text.ParserCombinators.Parsec
import           Text.ParserCombinators.Parsec.Expr
import           Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token    as Token

import           Control.Monad
import           Control.Monad.Except

import           GHC.Generics                           hiding (Infix, Prefix)
import           Generics.Simplistic.Deep
import           Generics.Simplistic.Deep.TH

-----------------------
-- * Parser

data BExpr = BoolConst Bool
           | Not BExpr
           | BBinary BBinOp BExpr BExpr
           | RBinary RBinOp AExpr AExpr
            deriving (Show , Eq)

data BBinOp = And | Or deriving (Show , Eq)

data RBinOp = Greater | Less | Equal deriving (Show , Eq)

data AExpr = Var String
           | IntConst Integer
           | Neg AExpr
           | ABinary ABinOp AExpr AExpr
           | ARange AExpr AExpr
             deriving (Show , Eq)

data ABinOp = Add
            | Subtract
            | Multiply
            | Reminder
            | Divide
            | Power
              deriving (Show , Eq)

data Stmt = Seq [Stmt]
          | Assign String AExpr
          | If BExpr Stmt Stmt
          | While BExpr Stmt
          | Skip
            deriving (Show , Eq)

type WhilePrims = '[ Integer , Bool , String ]

type WhileFam = '[ Stmt , [Stmt] , AExpr , BExpr , ABinOp , RBinOp , BBinOp ]

deriving instance Generic Stmt
deriving instance Generic AExpr
deriving instance Generic BExpr
deriving instance Generic ABinOp
deriving instance Generic BBinOp
deriving instance Generic RBinOp

deriveDeepFor ''WhilePrims ''WhileFam

dfromWhile :: Stmt -> SFix WhilePrims WhileFam Stmt
dfromWhile = dfrom

dtoWhile   :: SFix WhilePrims WhileFam Stmt -> Stmt
dtoWhile   = dto

-- ** Parser definition

languageDef =
  emptyDef { Token.commentStart    = "/*"
           , Token.commentEnd      = "*/"
           , Token.commentLine     = "//"
           , Token.identStart      = letter
           , Token.identLetter     = alphaNum
           , Token.reservedNames   = [ "if"
                                     , "then"
                                     , "else"
                                     , "while"
                                     , "do"
                                     , "skip"
                                     , "true"
                                     , "false"
                                     , "not"
                                     , "and"
                                     , "or"
                                     , "range"
                                     ]
           , Token.reservedOpNames = ["+", "-", "*", "^", "/", ":=" , "%"
                                     , "<", ">", "and", "or", "not" , "=="
                                     ]
           }
lexer = Token.makeTokenParser languageDef
identifier = Token.identifier lexer -- parses an identifier
reserved   = Token.reserved   lexer -- parses a reserved name
reservedOp = Token.reservedOp lexer -- parses an operator
parseBraces = Token.braces     lexer
parseParens = Token.parens     lexer -- parses surrounding parenthesis:
                                    --   parseParens p
                                    -- takes care of the parenthesis and
                                    -- uses p to parse what's inside them
integer    = Token.integer    lexer -- parses an integer
parseSemi  = Token.semi       lexer -- parses a parseSemicolon
whiteSpace = Token.whiteSpace lexer -- parses whitespace

whileParser :: Parser Stmt
whileParser = whiteSpace >> sequenceOfStmt

statement :: Parser Stmt
statement = statement'
        <|> parseBraces sequenceOfStmt

sequenceOfStmt =
  do list <- many1 statement'
     -- If there's only one statement return it without using Seq.
     return $ if length list == 1 then head list else Seq list

statement' :: Parser Stmt
statement' =   ifStmt
           <|> whileStmt
           <|> skipStmt   <* parseSemi
           <|> assignStmt <* parseSemi

ifStmt :: Parser Stmt
ifStmt =
  do reserved "if"
     cond  <- bExpression
     reserved "then"
     stmt1 <- statement
     reserved "else"
     If cond stmt1 <$> statement

whileStmt :: Parser Stmt
whileStmt =
  do reserved "while"
     cond <- bExpression
     reserved "do"
     While cond <$> statement

assignStmt :: Parser Stmt
assignStmt =
  do var  <- identifier
     reservedOp ":="
     Assign var <$> aExpression

skipStmt :: Parser Stmt
skipStmt = reserved "skip" >> return Skip

aExpression :: Parser AExpr
aExpression = buildExpressionParser aOperators aTerm

bExpression :: Parser BExpr
bExpression = buildExpressionParser bOperators bTerm

aOperators = [ [Prefix (reservedOp "-"   >> return Neg)          ]
             , [Infix  (reservedOp "^"   >> return (ABinary Power   )) AssocLeft]
             , [Infix  (reservedOp "*"   >> return (ABinary Multiply)) AssocLeft,
                Infix  (reservedOp "/"   >> return (ABinary Divide  )) AssocLeft,
                Infix  (reservedOp "%"   >> return (ABinary Reminder)) AssocLeft]
             , [Infix  (reservedOp "+"   >> return (ABinary Add     )) AssocLeft,
                Infix  (reservedOp "-"   >> return (ABinary Subtract)) AssocLeft]
             ]

bOperators = [ [Prefix (reservedOp "not" >> return Not)          ]
             , [Infix  (reservedOp "and" >> return (BBinary And     )) AssocLeft,
                Infix  (reservedOp "or"  >> return (BBinary Or      )) AssocLeft]
             ]

aTerm =  parseParens aExpression
     <|> fmap Var identifier
     <|> fmap IntConst integer
     <|> (reserved "range" >> liftM2 ARange aExpression aExpression)

bTerm =  parseParens bExpression
     <|> (reserved "true"  >> return (BoolConst True ))
     <|> (reserved "false" >> return (BoolConst False))
     <|> rExpression

rExpression =
  do a1 <- aExpression
     op <- relation
     RBinary op a1 <$> aExpression

relation =   (reservedOp ">" >> return Greater)
         <|> (reservedOp "<" >> return Less)
         <|> (reservedOp "==" >> return Equal)

parseString :: String -> Stmt
parseString str =
  case parse whileParser "" str of
    Left e  -> error $ show e
    Right r -> r

parseFile :: String -> ExceptT String IO Stmt
parseFile file =
  do program  <- lift $ readFile file
     case parse whileParser "" program of
       Left e  -> throwError (show e)
       Right r -> return r
