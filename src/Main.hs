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

-- | Common types
tint = VRec "int"
tbool = VRec "bool"

-- | A default tag environment (int/bool/list/tree/somefn)
defaultTagEnv s =
  case (s, reads s :: [(Integer, String)]) of
    ('"' : _, _) -> Just ("string", VUnit)
    ("true", _) -> Just ("bool", VUnit)
    ("false", _) -> Just ("bool", VUnit)
    ("Leaf", _) -> Just ("tree", tint)
    ("Node", _) -> Just ("tree", VPair (VRec "tree") (VRec "tree"))
    ("Nil", _) -> Just ("list", VUnit)
    ("Cons", _) -> Just ("list", VPair tint (VRec "intlist"))
    ("BoolFn", _) -> Just ("somefn", C (CArrow tbool (V tbool)))
    ("IntFn", _) -> Just ("somefn", C (CArrow tint (V tint)))
    (_, [(_,"")]) -> Just ("int", VUnit)
    _ -> Nothing

-- | A default label environment (stream/iset/insertunion)
defaultLabEnv s =
  case s of 
    "head" -> Just ("stream", V tint)
    "tail" -> Just ("stream", CRec "stream")
    "isEmpty" -> Just ("iset", V tbool)
    "contains" -> Just ("iset", CArrow tint (CRec "iset"))
    "insert" -> Just ("iset", CArrow tint (CRec "iset"))
    "union" -> Just ("iset", CArrow (C (CRec "iset")) (CRec "iset"))
    "Insert" -> Just ("insertunion", 
                      CArrow (VPair (C (CRec "iset")) tint)
                        (CRec "iset"))
    "Union" -> Just ("insertunion", 
                      CArrow (VPair (C (CRec "iset")) (C (CRec "iset")))
                        (CRec "iset"))

-- | A default environment 
defaultEnv = 
  [ (Syntax.Name "_prim_plus", C (CArrow tint (CArrow tint (V tint)))),
    (Syntax.Name "_prim_times", C (CArrow tint (CArrow tint (V tint)))),
    (Syntax.Name "_prim_minus", C (CArrow tint (CArrow tint (V tint)))),
    (Syntax.Name "_prim_eq", C (CArrow tint (CArrow tint (V tbool)))),
    (Syntax.Name "_prim_lt", C (CArrow tint (CArrow tint (V tbool)))) ]

loadTop :: Env -> [Syntax.TopLevelCmd] -> IO ()
loadTop env [] = putStrLn "Done.\n"
loadTop env (top : tops) = 
  case transTop env top of
    Nothing -> 
      putStrLn "TYPE ERROR:" >>
      print top >> 
      putStrLn "\n"
    Just (top', env') -> 
      if tcTop defaultTagEnv defaultLabEnv env top' 
      then print top' >>
           loadTop env' tops
      else putStrLn "ERROR TYPECHECKING IL:" >>
           print top'

load :: String -> IO ()
load s = do
  f <- readFile s
  let s = decomment f
  loadTop defaultEnv (runParser s pTopLevel s)

main :: IO ()
main = do
  putStrLn "Welcome to HLevy\n"

  let testExpr = runParser "input" pExpr "3+4*2-100"

  print testExpr

