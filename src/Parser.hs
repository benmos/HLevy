{-# LANGUAGE FlexibleContexts, Rank2Types, ImpredicativeTypes #-}
--
-- HLevy
-- Copyright (c) 2012 - Ben Moseley
--
module Parser(
  pExpr
)
where

import Syntax
import UUParsingLibFixes

import Control.Applicative
import Data.Char
import Data.List
import Text.ParserCombinators.UU hiding (Apply)
import Text.ParserCombinators.UU.Utils hiding (runParser, execParser)
import Text.ParserCombinators.UU.BasicInstances (LineColPos(..),Error(..),Str,Parser,pSym,createStr,
                                                show_expecting, pSymInsert, Insertion(..), pMunch,
                                                pRange, ParserTrafo)
import Text.Printf

type UUP a = P (Str Char String LineColPos) a

operators :: [[(UUP String, String -> Expr -> Expr -> Expr)]]
operators = [
              [(pSymbol "<", const Less), (pSymbol "=", const Equal)],
              [(pSymbol "+", const Plus), (pSymbol "-", const Minus)],
              [(pSymbol "*", const Times)],
              [(fmap unName $ (pSymbol "to" *> pIdentifier <* pSymbol "in"), flip To . Name)]
            ]

same_prio :: [(UUP b, b -> a)] -> UUP a
same_prio ops = foldr (<|>) empty [ op <$> lexeme p | (p, op) <- ops]

pExpr :: UUP Expr
pExpr = pApp <|>
        pBinOpExpr <|>
        pLetExpr
    where
      pLetExpr   = Let <$> (pSymbol "let" *> pIdentifier) <*>
                           (pSymbol "="   *> pExpr) <*>
                           (pSymbol "in"  *> pExpr)
      pBinOpExpr = foldr pChainl (pNonAppExpr) (map same_prio operators)

pApp :: UUP Expr
pApp = Apply <$> pInitialApp <*> pChainl (pure Apply) pNonAppExpr <|>
       pNonAppExpr

pInitialApp :: UUP Expr
pInitialApp = (Thunk  <$ pSymbol "thunk"  <|>
               Force  <$ pSymbol "force"  <|>
               Return <$ pSymbol "return") <*> pNonAppExpr

pNonAppExpr :: UUP Expr
pNonAppExpr = Var <$> pIdentifier <|>
              EBool True  <$ pSymbol "true" <|>
              EBool False <$ pSymbol "false" <|>
              EInt <$> pNatural <|>
              pParens pExpr

pIdentifierRaw :: UUP Name
pIdentifierRaw = Name <$> ((:) <$> pRange ('a','z') <*> pMunch isAlphaNum `micro` 1)

pIdentifier :: UUP Name
pIdentifier = lexeme pIdentifierRaw

pBoolOp :: UUP (Expr -> Expr -> Expr)
pBoolOp = Less <$ pSym '<'

testExpr :: Expr
testExpr = runParser "input" pExpr "3+4*2-100"

