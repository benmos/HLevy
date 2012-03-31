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

-- | A default tag environment (int/bool/list/tree/somefn)
defaultTagEnv s =
  case (s, reads s :: [(Integer, String)]) of
    ('"' : _, _) -> Just ("string", VUnit)
    ("true", _) -> Just ("bool", VUnit)
    ("false", _) -> Just ("bool", VUnit)
    ("Leaf", _) -> Just ("tree", VRec "int")
    ("Node", _) -> Just ("tree", VPair (VRec "tree") (VRec "tree"))
    ("Nil", _) -> Just ("list", VUnit)
    ("Cons", _) -> Just ("list", VPair (VRec "int") (VRec "intlist"))
    ("BoolFn", _) -> Just ("somefn", C (CArrow (VRec "bool") (V (VRec "bool"))))
    ("IntFn", _) -> Just ("somefn", C (CArrow (VRec "int") (V (VRec "int"))))
    (_, [(_,"")]) -> Just ("int", VUnit)
    _ -> Nothing

-- | A default label environment (stream/iset/insertunion)
defaultLabEnv s =
  case s of 
    "head" -> Just ("stream", V (VRec "int"))
    "tail" -> Just ("stream", CRec "stream")
    "isEmpty" -> Just ("iset", V (VRec "bool"))
    "contains" -> Just ("iset", CArrow (VRec "int") (CRec "iset"))
    "insert" -> Just ("iset", CArrow (VRec "int") (CRec "iset"))
    "union" -> Just ("iset", CArrow (C (CRec "iset")) (CRec "iset"))
    "Insert" -> Just ("insertunion", 
                      CArrow (VPair (C (CRec "iset")) (VRec "int"))
                        (CRec "iset"))
    "Union" -> Just ("insertunion", 
                      CArrow (VPair (C (CRec "iset")) (C (CRec "iset")))
                        (CRec "iset"))

loadTop :: Env -> [Syntax.TopLevelCmd] -> IO ()
loadTop env [] = putStrLn "Done.\n"
loadTop env (top : tops) = 
  case transTop env top of
    Nothing -> 
      putStrLn "TYPE ERROR:" >>
      print top >> 
      putStrLn "\n"
    Just (top', env') -> do
      () <- print top'
      loadTop env' tops

load :: String -> IO ()
load s = do
  f <- readFile s
  let s = decomment f
  loadTop [] (runParser s pTopLevel s)

main :: IO ()
main = do
  putStrLn "Welcome to HLevy\n"

  let testExpr = runParser "input" pExpr "3+4*2-100"

  print testExpr

