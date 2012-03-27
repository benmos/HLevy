{-# LANGUAGE FlexibleContexts, Rank2Types, ImpredicativeTypes #-}
--
-- HLevy
-- Copyright (c) 2012 - Ben Moseley
--
module Parser(
  decomment,
  pType,
  pExpr,
  pTopLevel
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

decomment :: String -> String
decomment = dropWhile isSpace . unlines . map (takeWhile (/='#')) . lines

pExpr :: UUP Expr
pExpr = -- pInitialExpr <|>
        pBinOpExpr

pInitialExpr :: UUP Expr
pInitialExpr = pApp        <|>
               pInitialApp <|>
               pLetExpr    <|>
               pIfExpr     <|>
               pFunExpr    <|>
               pRecExpr
    where
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
                           (pSymbol "is"   *> pExpr)

pBinOpExpr :: UUP Expr
pBinOpExpr = foldr pChainl (pInitialExpr {- InitialApp -}) (map same_prio operators)
    where
      operators :: [[(UUP String, String -> Expr -> Expr -> Expr)]]
      operators = [
                    [(pSymbol "<", const Less), (pSymbol "=", const Equal)],
                    [(pSymbol "+", const Plus), (pSymbol "-", const Minus)],
                    [(pSymbol "*", const Times)],
                    [(fmap unName $ (pSymbol "to" *> pIdentifier <* pSymbol "in"), flip To . Name)]
                  ]

      same_prio :: [(UUP b, b -> a)] -> UUP a
      same_prio ops = foldr (<|>) empty [ op <$> lexeme p | (p, op) <- ops]


pApp :: UUP Expr
pApp = (foldl1 Apply .) . (:) <$> pInitialApp <*> pList1 pNonAppExpr

pInitialApp :: UUP Expr
pInitialApp = pNonAppExpr <|>
              (Thunk  <$ pSymbol "thunk"  <|>
               Force  <$ pSymbol "force"  <|>
               Return <$ pSymbol "return") <*> pNonAppExpr

pNonAppExpr :: UUP Expr
pNonAppExpr = EBool True  <$  pSymbol "true"  <<|>
              EBool False <$  pSymbol "false" <<|>
              EInt        <$> lexeme pIntRaw  <<|>
              Var         <$> pIdentifier <|>
              pParens pExpr
    where
      pIntRaw :: (Num a) => Parser a
      pIntRaw = pOptionalNeg <*> pNaturalRaw <?> "Integer"

      pOptionalNeg :: (Num a) => Parser (a -> a)
      pOptionalNeg = ((negate <$ (pSym '-')) `opt` id)



pIdentifierRaw :: UUP Name
pIdentifierRaw = Name <$> ((:) <$> pInitial <*> pMunch isSubseq `micro` 1)
    where
      pInitial = pRange ('a','z') <|> pRange ('A','Z') <|> pSym '_'
      isSubseq = (||) <$> isAlphaNum <*> (`elem` "'_")

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

pDef :: UUP TopLevelCmd
pDef = Def    <$> (pSymbol "let" *> pIdentifier) <*> (pSymbol "=" *> pExpr) <|>
       RunDef <$> (pSymbol "do"  *> pIdentifier) <*> (pSymbol "=" *> pExpr)

pCmd :: UUP TopLevelCmd
pCmd = Use . unName <$> (pSymbol "use" *> pIdentifier) <|> -- is pIdentifier right?
       Quit         <$ pSymbol "quit"

pTopLevelCmd :: UUP TopLevelCmd
pTopLevelCmd = pDef <|> pCmd <|> Expr <$> pExpr

pTopLevel :: UUP [TopLevelCmd]
pTopLevel = pListSep_ng (pSymbol ";;") pTopLevelCmd <* pSymbol ";;"

------------------------------------------------------------------------

testExpr :: Expr
testExpr = runParser "input" pExpr "if true then 3+4*2-100 else f 4"

testExpr2 :: Expr
testExpr2 = runParser "input" pExpr "fun myf : U int -> f 4"

testType :: LType
testType = runParser "input" pType "bool -> (int -> U F bool)"

