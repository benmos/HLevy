-- 
-- HLevy
-- Default environments 
-- 

module DefaultEnv(
  defaultTagEnv,
  defaultLabEnv, 
  defaultEnv,
  defaultVEnv
)
where 

import qualified Syntax
import IL
import Exec

-- | Common types
tint = VRec "int"
tbool = VRec "bool"
tstring = VRec "string"

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
    (Syntax.Name "_prim_lt", C (CArrow tint (CArrow tint (V tbool)))),
    (Syntax.Name "print", C (CArrow tstring (V VUnit))) ]

wi :: (Int -> IO Terminal) -> IO Terminal
wi f = return (TPrim (\(RTag i RUnit) -> f (read i)))

ti :: Int -> IO Terminal
ti i = return (TValue (RTag (show i) RUnit))

tb :: Bool -> IO Terminal
tb True = return (TValue (RTag "true" RUnit))
tb False = return (TValue (RTag "false" RUnit))

defaultVEnv = 
  [ (Syntax.Name "_prim_plus", RPrim (wi (\x -> wi (\y -> ti (x + y))))),
    (Syntax.Name "_prim_times", RPrim (wi (\x -> wi (\y -> ti (x * y))))),
    (Syntax.Name "_prim_minus", RPrim (wi (\x -> wi (\y -> ti (x - y))))),
    (Syntax.Name "_prim_eq", RPrim (wi (\x -> wi (\y -> tb (x == y))))),
    (Syntax.Name "_prim_lt", RPrim (wi (\x -> wi (\y -> tb (x < y))))),
    (Syntax.Name "print", RPrim (return 
                                  (TPrim 
                                    (\(RTag ('"' : s) RUnit) -> 
                                      putStrLn s >> return (TValue RUnit))))) ]
