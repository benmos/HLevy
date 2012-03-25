{-# LANGUAGE FlexibleContexts, Rank2Types, ImpredicativeTypes #-}
--
-- HLevy
-- Copyright (c) 2012 - Ben Moseley
--
module UUParsingLibFixes(
  execParser,
  runParser
)
where

import Control.Applicative
import Data.Char
import Data.List
import Text.ParserCombinators.UU hiding (Apply)
import Text.ParserCombinators.UU.Utils hiding (runParser, execParser)
import Text.ParserCombinators.UU.BasicInstances (LineColPos(..),Error(..),Str,Parser,pSym,createStr,
                                                show_expecting, pSymInsert, Insertion(..), pMunch,
                                                pRange, ParserTrafo)
import Text.Printf

------------------------------------------------------------------------
-- UU Parsing Lib fns with more polymorphic types
-- ... to be removed when uu-parsinglib is fixed with these relaxed types

execParser :: P (Str Char String LineColPos) a -> String -> (a, [Error LineColPos])
execParser p = parse_h ((,) <$> p <*> pEnd) . createStr (LineColPos 0 0 0)

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