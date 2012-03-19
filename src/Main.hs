--
-- HLevy
-- Copyright (c) 2012 - Ben Moseley
--
module Main where

import Parser
import Text.ParserCombinators.UU.Utils

main :: IO ()
main = do
  putStrLn "Welcome to HLevy\n"

  let testExpr = runParser "input" pExpr "3+4*2-100"

  print testExpr

