--
-- HLevy
-- Copyright (c) 2012 - Ben Moseley
--
module Main where

import Parser
import UUParsingLibFixes

import Text.ParserCombinators.UU.Utils hiding (runParser)
import qualified Syntax
import IL

load :: String -> IO [Syntax.TopLevelCmd]
load s = do
  f <- readFile s
  let s = decomment f
  let p = runParser s pTopLevel s
  return p

main :: IO ()
main = do
  putStrLn "Welcome to HLevy\n"

  let testExpr = runParser "input" pExpr "3+4*2-100"

  print testExpr

