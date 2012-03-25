{-# LANGUAGE FlexibleContexts, Rank2Types, ImpredicativeTypes #-}
--
-- HLevy
-- Copyright (c) 2012 - Ben Moseley
--
module Parser(
  pType,
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
pExpr = pApp        <|>
        pBinOpExpr  <|>
        pLetExpr    <|>
        pIfExpr     <|>
        pFunExpr    <|>
        pRecExpr
    where
      pBinOpExpr = foldr pChainl (pNonAppExpr) (map same_prio operators)
      pLetExpr   = Let <$> (pSymbol "let" *> pIdentifier) <*>
                           (pSymbol "="   *> pExpr) <*>
                           (pSymbol "in"  *> pExpr)
      pIfExpr    = If  <$> (pSymbol "if"   *> pExpr) <*>
                           (pSymbol "then" *> pExpr) <*>
                           (pSymbol "else" *> pExpr)
      pFunExpr   = Fun <$> (pSymbol "fun"  *> pIdentifier) <*>
                           (pSymbol ":"    *> pType) <*>
                           (pSymbol "->"   *> pExpr)
      pRecExpr   = Rec <$> (pSymbol "rec"  *> pIdentifier) <*>
                           (pSymbol ":"    *> pType) <*>
                           (pSymbol "->"   *> pExpr)

pApp :: UUP Expr
pApp = Apply <$> pInitialApp <*> pChainl (pure Apply) pNonAppExpr

pInitialApp :: UUP Expr
pInitialApp = pNonAppExpr <|>
              (Thunk  <$ pSymbol "thunk"  <|>
               Force  <$ pSymbol "force"  <|>
               Return <$ pSymbol "return") <*> pNonAppExpr

pNonAppExpr :: UUP Expr
pNonAppExpr = EBool True  <$  pSymbol "true"  <<|>
              EBool False <$  pSymbol "false" <<|>
              EInt        <$> pNatural        <<|>
              Var         <$> pIdentifier <|>
              pParens pExpr

pIdentifierRaw :: UUP Name
pIdentifierRaw = Name <$> ((:) <$> pRange ('a','z') <*> pMunch isAlphaNum `micro` 1)

pIdentifier :: UUP Name
pIdentifier = lexeme pIdentifierRaw

------------------------------------------------------------------------

pType :: UUP LType
pType = pNonArrType <|>
        CArrow <$> pNonArrType <* pSymbol "->" <*> pType

pNonArrType :: UUP LType
pNonArrType = VInt    <$ pSymbol "int" <|>
              VBool   <$ pSymbol "bool" <|>
              VForget <$ pSymbol "U" <*> pType <|>
              CFree   <$ pSymbol "F" <*> pType <|>
              pParens pType

------------------------------------------------------------------------

testExpr :: Expr
testExpr = runParser "input" pExpr "if true then 3+4*2-100 else f 4"

testExpr2 :: Expr
testExpr2 = runParser "input" pExpr "fun myf : U int -> f 4"

testType :: LType
testType = runParser "input" pType "bool -> (int -> U F bool)"

