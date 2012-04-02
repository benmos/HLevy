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
import Exec
import DefaultEnv

loadTop :: Env -> VEnv -> [Syntax.TopLevelCmd] -> IO ()
loadTop env venv [] = putStrLn "Done.\n"
loadTop env venv (top : tops) = 
  case transTop env top of
    Nothing -> 
      putStrLn "TYPE ERROR:" >>
      print top >> 
      putStrLn "\n"
    Just (top', env') -> 
      if not (tcTop defaultTagEnv defaultLabEnv env top')
      then putStrLn "ERROR TYPECHECKING IL:" >> print top'
      else runTop venv top' >>= \venv' -> loadTop env' venv' tops

load :: String -> IO ()
load s = do
  f <- readFile s
  let s = decomment f
  loadTop defaultEnv defaultVEnv (runParser s pTopLevel s)

main :: IO ()
main = do
  putStrLn "Welcome to HLevy\n"

  let testExpr = runParser "input" pExpr "3+4*2-100"

  print testExpr

