{-# LANGUAGE FlexibleContexts #-}
--
-- HLevy
-- Copyright (c) 2012 - Ben Moseley
--
module Parser(
  pExpr
)
where

import Syntax

import Control.Applicative
import Data.Char

import Text.ParserCombinators.UU
import Text.ParserCombinators.UU.Utils
import Text.ParserCombinators.UU.BasicInstances (LineColPos(..),Error(..),Str,Parser,pSym,createStr,
                                                show_expecting, pSymInsert, Insertion(..), pMunch,
                                                pRange)
import Text.Printf

operators :: [[(Char, Expr -> Expr -> Expr)]]
operators = [
              [('+', Plus), ('-', Minus)],
              [('*', Times)] -- ,
              -- [('^', (^))]
            ]

same_prio :: [(Char, a)] -> Parser a
same_prio ops = foldr (<|>) empty [ op <$ lexeme (pSym c) | (c, op) <- ops]


pIntExpr :: Parser Expr
pIntExpr = foldr pChainl (EInt <$> pNatural <|> pParens pIntExpr) (map same_prio operators)

pExpr :: Parser Expr
pExpr = Var <$> pIdentifier <|> pIntExpr <|> pBoolExpr

pIdentifierRaw :: Parser Name
pIdentifierRaw = Name <$> ((:) <$> pRange ('a','z') <*> pMunch isAlphaNum `micro` 1)

pIdentifier :: Parser Name
pIdentifier = lexeme pIdentifierRaw

pBoolExpr :: Parser Expr
pBoolExpr = pBoolConst <|> (\x op y -> x `op` y) <$> pIntExpr <*> pBoolOp <*> pIntExpr
    where
      pBoolConst = EBool True  <$ pSymbol "true" <|>
                   EBool False <$ pSymbol "false"

pBoolOp :: Parser (Expr -> Expr -> Expr)
pBoolOp = Less <$ pSym '<'

testExpr :: Expr
testExpr = runParser "input" pExpr "3+4*2-100"
