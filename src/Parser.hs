{-# LANGUAGE FlexibleContexts, Rank2Types #-}
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

newtype WrappedParser a = WrappedParser { unWrapParser :: Parser a }

operators :: [[(WrappedParser String, String -> Expr -> Expr -> Expr)]]
operators = [
              [(wrapSym "<", const Less), (wrapSym "=", const Equal)],
              [(wrapSym "+", const Plus), (wrapSym "-", const Minus)],
              [(wrapSym "*", const Times)],
              [(WrappedParser $ fmap unName $ (pSymbol "to" *> pIdentifier <* pSymbol "in"), flip To . Name)]
            ]
  where
    wrapSym s = WrappedParser $ pSymbol $ s

same_prio :: [(WrappedParser b, b -> a)] -> Parser a
same_prio ops = foldr (<|>) empty [ op <$> lexeme p | (WrappedParser p, op) <- ops]

pExpr :: Parser Expr
pExpr = pUnaryOpExpr <|>
              pBinOpExpr <|>
              pLetExpr <|>
              Var <$> pIdentifier
    where
      pLetExpr = Let <$> (pSymbol "let" *> pIdentifier) <*>
                         (pSymbol "="   *> pExpr) <*>
                         (pSymbol "in"  *> pExpr)

      pTo :: Parser Expr
      pTo = To <$> pExpr <*> (pSymbol "to" *> pIdentifier <* pSymbol "in") <*> pExpr

      pBinOpExpr   = foldr pChainl (pAtom <|> pParens pExpr) (map same_prio operators)
      pUnaryOpExpr = ((Thunk  <$ pSymbol "thunk"  <|>
                       Force  <$ pSymbol "force"  <|>
                       Return <$ pSymbol "return") <*> pExpr)

pAtom :: Parser Expr
pAtom = EInt <$> pNatural <|>
        EBool True  <$ pSymbol "true" <|>
        EBool False <$ pSymbol "false"

pIdentifierRaw :: Parser Name
pIdentifierRaw = Name <$> ((:) <$> pRange ('a','z') <*> pMunch isAlphaNum `micro` 1)

pIdentifier :: Parser Name
pIdentifier = lexeme pIdentifierRaw

pBoolOp :: Parser (Expr -> Expr -> Expr)
pBoolOp = Less <$ pSym '<'

testExpr :: Expr
testExpr = runParser "input" pExpr "3+4*2-100"
