{-# LANGUAGE FlexibleContexts, LiberalTypeSynonyms #-}
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
import Text.ParserCombinators.UU.Utils hiding (execParser, runParser)
import Text.ParserCombinators.UU.BasicInstances (LineColPos(..),Error(..),Str,Parser,pSym,createStr,
                                                show_expecting)
import qualified Data.ListLike as LL
import Text.Printf

type UUP a = P (Str Char String LineColPos) a

operators :: [[(Char, Expr -> Expr -> Expr)]]
operators = [
              [('+', Plus), ('-', Minus)],
              [('*', Times)] -- ,
              -- [('^', (^))]
            ]

same_prio :: [(Char, a)] -> UUP a
same_prio ops = foldr (<|>) empty [ op <$ pSym c | (c, op) <- ops]

pExpr :: UUP Expr
pExpr = foldr pChainl (EInt <$> pNatural <|> pParens pExpr) (map same_prio operators)

testExpr :: Expr
testExpr = runParser "input" pExpr "3+4*2-1"

-- Needs Rank2Types ...? :
-- type ParserX a = forall loc state . (IsLocationUpdatedBy loc Char, LL.ListLike state Char) =>
--                  P (Str Char state loc) a


------------------------------------------------------------------------
-- UUParsing Utils
--
-- These two functions are identical to the ones in Text.ParserCombinators.UU.Utils
-- except that they have more sensible types... this needs to be pushed upstream.

-- | The lower-level interface. Returns all errors.
execParser :: LL.ListLike state Char =>
              P (Str Char state LineColPos) a -> state -> (a, [Error LineColPos])
execParser p = parse_h ((,) <$> p <*> pEnd) . createStr (LineColPos 0 0 0)

-- | The higher-level interface. (Calls 'error' with a simplified error).
--   Runs the parser; if the complete input is accepted without problems  return the
--   result else fail with reporting unconsumed tokens
runParser :: String -> P (Str Char String LineColPos) a -> String -> a
runParser inputName p s | (a,b) <- execParser p s =
    if null b
    then a
    else error (printf "Failed parsing '%s' :\n%s\n" inputName (pruneError s b))
         -- We do 'pruneError' above because otherwise you can end
         -- up reporting huge correction streams, and that's
         -- generally not helpful... but the pruning does discard info...
    where -- | Produce a single simple, user-friendly error message
          pruneError :: String -> [Error LineColPos] -> String
          pruneError _ [] = ""
          pruneError _ (DeletedAtEnd x     : _) = printf "Unexpected '%s' at end." x
          pruneError s (Inserted _ pos exp : _) = prettyError s exp pos
          pruneError s (Deleted  _ pos exp : _) = prettyError s exp pos
          prettyError :: String -> [String] -> LineColPos -> String
          prettyError s exp p@(LineColPos line c abs) = printf "Expected %s at %s :\n%s\n%s\n%s\n"
                                                           (show_expecting p exp)
                                                           (show p)
                                                           aboveString
                                                           inputFrag
                                                           belowString
                             where
                                s' = map (\c -> if c=='\n' || c=='\r' || c=='\t' then ' ' else c) s
                                aboveString = replicate 30 ' ' ++ "v"
                                belowString = replicate 30 ' ' ++ "^"
                                inputFrag   = replicate (30 - c) ' ' ++ (take 71 $ drop (c - 30) s')

------------------------------------------------------------------------
