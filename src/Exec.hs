-- 
-- HLevy operational semantics
-- 
-- Follows Bauer's implementation in using closures instead of explicit
-- substitution, but separates the runtime representation of values (RunVal)
-- from the runtime representation of computations (Terminal)
-- 

module Exec (
  -- * Types
  VEnv,
  RunVal(..),
  Terminal(..),

  -- * Functions
  runTop
)
where

import qualified Syntax
import Control.Applicative
import IL

data RunVal = RThunk VEnv Cmd
            | RPrim (IO Terminal)
            | RTag String RunVal
            | RPair RunVal RunVal
            | RUnit

instance Show RunVal where show (RThunk _ _) = "<thunk>"
                           show (RPrim _) = "<primitive thunk>"
                           show (RTag s RUnit) = s
                           show (RTag s rv) = s ++ " " ++ show rv
                           show (RPair rv1 rv2) =
                             "(" ++ show rv1 ++ "," ++ show rv2 ++ ")"
                           show (RUnit) = "()"
                           
data Terminal = TValue RunVal                  -- produce V
              | TFun VEnv Syntax.Name Cmd      -- \x.cmd
              | TPrim (RunVal -> IO Terminal)  -- \x.cmd
              | TRecord VEnv [(String, Cmd)]   -- {l1:cmd1,...}

instance Show Terminal where show (TValue rv) = "F " ++ show rv
                             show (TFun _ x _) = "fn " ++ show x ++ " -> ..."
                             show (TPrim _) = "<primitive function>"
                             show (TRecord _ _) = "{...}"

type VEnv = [(Syntax.Name, RunVal)]


--
find :: Eq a => a -> [(a, b)] -> IO b
find xs x =
  case lookup xs x of
    Nothing -> fail "Lookup failed"
    Just y -> return y

-- | Turns a value into a closed value
runValue :: VEnv -> Value -> IO RunVal
runValue env (Var x) = find x env
runValue env (Tag s v) =
  RTag s <$> runValue env v
runValue env (Thunk cmd) = return (RThunk env cmd)
runValue env (Pair v1 v2) = 
  runValue env v1 >>= \rv1 -> RPair rv1 <$> runValue env v2
  -- attn benmos: is there a more idiomatic way to write this? -- rjs
runValue env Unit = return RUnit
runValue env (Compute cmd) = do
  TValue rv <- runCmd env cmd
  return rv 


-- | Checks whether an individual pattern matches a value; extends context
match :: RunVal -> Pat -> VEnv -> Maybe VEnv
match rv (PVar x) env = return ((x, rv) : env)
match (RTag c rv) (PTag c' pat) env =
  if c == c' then match rv pat env else Nothing
match RUnit PUnit env = return env
match (RPair rv1 rv2) (PPair p1 p2) env = match rv1 p1 env >>= match rv2 p2


-- | Runs a case analysis, produces nonexhaustive pattern match failure
runCase :: VEnv -> RunVal -> [(Pat, Cmd)] -> IO Terminal
runCase env rv [] = fail "Nonexhaustive pattern match"
runCase env rv ((pat, cmd) : arms) = 
  case match rv pat [] of 
    Nothing -> runCase env rv arms
    Just env' -> runCmd (env' ++ env) cmd


-- | Turns a command into what Levy calls a "terminal computation"
runCmd :: VEnv -> Cmd -> IO Terminal
runCmd env (Print v cmd) = do
  RTag ('"' : s) RUnit <- runValue env v
  () <- putStrLn s
  runCmd env cmd
runCmd env (Do x cmd1 cmd2) = do
  TValue rv1 <- runCmd env cmd1
  runCmd ((x, rv1) : env) cmd2
runCmd env (Case v arms) = do
  rv <- runValue env v
  runCase env rv arms
runCmd env (Fun x _ cmd) = return (TFun env x cmd)
runCmd env (Rec x _ cmd) = runCmd ((x, RThunk env cmd) : env) cmd
runCmd env (Force v) = do
  term <- runValue env v
  case term of
    RThunk env cmd -> runCmd env cmd
    RPrim term' -> term'
runCmd env (Return v) = TValue <$> runValue env v
runCmd env (Apply cmd v) = do
  f <- runCmd env cmd
  rv <- runValue env v
     {- Effectful values can confuse us a bit here. We'd like to runValue 
        on the value first, in the current context, but we are prevented
        from doing so by the fact that we committed to left-to-right
        evaluation ordering. An OCaml-like right-to-left evaluation order
        would prevent this potential confusion, though the right solution
        is to write the actual IL pass that makes the order of evaluation for
        effectful values explicit; in that case we can evaluate values outside
        the IO monad. -}
  runApply f rv
runCmd env (Project cmd l) = do
  TRecord env fields <- runCmd env cmd
  find l fields >>= runCmd env 
runCmd env (Record fields) = return (TRecord env fields)

-- | Application that can handle primitive functions
runApply :: Terminal -> RunVal -> IO Terminal
runApply (TFun env x cmd) rv = runCmd ((x, rv) : env) cmd
runApply (TPrim f) rv = f rv

-- | Run a toplevel expression
runTop :: VEnv -> TopLevelCmd -> IO VEnv
runTop env (TopCmd cmd) = 
  runCmd env cmd >>= \term ->
  putStrLn ("Computation: " ++ show term) >>
  return env
runTop env (TopValue v) = 
  runValue env v >>= \rv ->
  putStrLn ("Value: " ++ show rv) >>
  return env
runTop env (TopLet x t v) =
  runValue env v >>= \rv ->
  putStrLn (show x ++ " : " ++ show t ++ " = " ++ show rv) >>
  return ((x, rv) : env)
runTop env (TopDo x t cmd) = 
  runCmd env cmd >>= \(TValue rv) ->
  putStrLn (show x ++ " : " ++ show t ++ " = " ++ show rv) >>
  return ((x, rv) : env)