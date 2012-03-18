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

import Text.ParserCombinators.UU
import Text.ParserCombinators.UU.Utils
import Text.ParserCombinators.UU.BasicInstances (LineColPos(..),Error(..),Str,Parser,pSym,createStr,
                                                show_expecting, pSymInsert, Insertion(..))
import Text.Printf

type UUP a = P (Str Char String LineColPos) a

operators :: [[(Char, Expr -> Expr -> Expr)]]
operators = [
              [('+', Plus), ('-', Minus)],
              [('*', Times)] -- ,
              -- [('^', (^))]
            ]

same_prio :: [(Char, a)] -> Parser a
same_prio ops = foldr (<|>) empty [ op <$ pSym c | (c, op) <- ops]

pExpr :: Parser Expr
pExpr = foldr pChainl (EInt <$> pNatural <|> pParens pExpr) (map same_prio operators)

testExpr :: Expr
testExpr = runParser "input" pExpr "3+4*2-100"
