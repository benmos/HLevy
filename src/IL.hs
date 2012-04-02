module IL (
  -- * Types
  VType(..),
  CType(..),
  Pat(..),
  Value(..),
  Cmd(..),
  TopLevelCmd(..),
  Env,
  TagEnv,
  LabEnv,

  -- * Functions
  tcCmd,
  tcValue,
  tcTop,
  transTop
)
where

import Data.Maybe
import Control.Applicative
import qualified Syntax

data VType = VRec String
           | VPair VType VType
           | VUnit
           | C CType
             deriving (Eq,Ord)

instance Show VType where show (VRec s) = s
                          show (VPair t1 t2) = 
                            "(" ++ show t1 ++ "," ++ show t2 ++ ")"
                          show (VUnit) = "()"
                          show (C t) = "U " ++ show t

data CType = CRec String
           | CArrow VType CType
           | V VType
             deriving (Eq,Ord)

instance Show CType where show (CRec s) = s
                          show (CArrow (C t1) t2) = 
                            "(U " ++ show t1 ++ ") -> " ++ show t2 
                          show (CArrow t1 t2) = 
                            show t1 ++ " -> " ++ show t2 
                          show (V t) = "F " ++ show t

data Pat   = PVar Syntax.Name
           | PTag String Pat
           | PPair Pat Pat
           | PUnit
             deriving (Eq,Ord,Show)

data Value = Var Syntax.Name            -- x
           | Tag String Value           -- Foo v
           | Thunk Cmd                  -- thunk cmd
           | Pair Value Value           -- v1, v2
           | Unit                       -- ()
           | Compute Cmd                -- < cmd >
             deriving (Eq,Ord,Show)

data Cmd   = Do Syntax.Name Cmd Cmd     -- do x <- cmd cmd
           | Case Value [ (Pat, Cmd) ]  -- case v of | p1 -> cmd1 | pn -> cmdn
           | Fun Syntax.Name VType Cmd  -- fn x:t -> cmd
           | Rec Syntax.Name CType Cmd  -- rec x:t is cmd
           | Force Value                -- force v
           | Return Value               -- return v
           | Apply Cmd Value            -- apply cmd v
           | Project Cmd String         -- cmd.foo
           | Record [ (String, Cmd) ]   -- { l1=cmd1, ..., ln=cmdn }
             deriving (Eq,Ord,Show)

data TopLevelCmd
           = TopCmd (Cmd, CType)              -- cmd
           | TopValue (Value, VType)          -- v <=== Note, this is dumb?
           | TopLet Syntax.Name VType Value   -- let x : t = v
           | TopDo  Syntax.Name VType Cmd     -- do x : V t <- cmd
             deriving (Eq,Ord,Show)

type Env = [(Syntax.Name, VType)]
type TagEnv = String -> Maybe (String, VType)
type LabEnv = String -> Maybe (String, CType)

inCtx :: Syntax.Name -> Env -> Bool
inCtx x [] = False
inCtx x ((y, _) : ctx) = x == y || inCtx x ctx

-- | Typechecking a pattern against a type produces an environment 
tcPat :: TagEnv -> Env -> Pat -> VType -> Maybe Env
tcPat tenv env (PVar x) t = 
  -- Can't bind a variable in a pattern more than once
  if inCtx x env then empty else return ((x, t) : env)
tcPat tenv env (PTag c pat) (VRec a) = do
  (a', t) <- tenv c 
  if a /= a' then empty
  else tcPat tenv env pat t
tcPat tenv env (PPair p1 p2) (VPair t1 t2) = do
  env' <- tcPat tenv env p1 t1
  tcPat tenv env' p2 t2
tcPat tenv env PUnit VUnit = return env

-- | Typechecking a Value
tcValue :: TagEnv -> LabEnv -> Env -> Value -> Maybe VType
tcValue tenv lenv env (Var x) = lookup x env
tcValue tenv lenv env (Tag c v) = do
  (a, t) <- tenv c
  t' <- tcValue tenv lenv env v
  if t == t' then return (VRec a) else empty
tcValue tenv lenv env (Thunk cmd) = C <$> tcCmd tenv lenv env cmd
tcValue tenv lenv env (Pair v1 v2) = do
  t1 <- tcValue tenv lenv env v1
  t2 <- tcValue tenv lenv env v2
  return (VPair t1 t2)
tcValue tenv lenv env Unit = return VUnit
tcValue tenv lenv env (Compute cmd) = do
  t <- tcCmd tenv lenv env cmd 
  case t of
    V t -> return t
    _ -> empty

-- | Typechecking a Cmd
tcCmd :: TagEnv -> LabEnv -> Env -> Cmd -> Maybe CType
tcCmd tenv lenv env (Do x cmd1 cmd2) = do
  t1 <- tcCmd tenv lenv env cmd1
  case t1 of 
    V t -> tcCmd tenv lenv ((x, t) : env) cmd2
    _ -> empty
tcCmd tenv lenv env (Case v arms) = do
  t <- tcValue tenv lenv env v
  tcArms tenv lenv env t arms
tcCmd tenv lenv env (Fun x t cmd) = 
  CArrow t <$> tcCmd tenv lenv ((x, t) : env) cmd 
tcCmd tenv lenv env (Rec x t cmd) = do
  t' <- tcCmd tenv lenv ((x, C t) : env) cmd
  if t == t' then return t else empty
tcCmd tenv lenv env (Force v) = do
  t <- tcValue tenv lenv env v 
  case t of 
    C t -> return t
    _ -> empty
tcCmd tenv lenv env (Return v) = V <$> tcValue tenv lenv env v
tcCmd tenv lenv env (Apply cmd v) = do
  t' <- tcCmd tenv lenv env cmd
  t <- tcValue tenv lenv env v
  case t' of 
     CArrow t1 t2 -> if t == t1 then return t2 else empty
     _ -> empty
tcCmd tenv lenv env (Project cmd proj) = do
  t <- tcCmd tenv lenv env cmd
  (a, t') <- lenv proj
  case t of 
    CRec a' -> if a == a' then return t' else empty
    _ -> empty
tcCmd tenv lenv env (Record fields) = CRec <$> tcFields tenv lenv env fields

-- | Typechecking the arms of a pattern
tcArms :: TagEnv -> LabEnv -> Env 
           -> VType -> [(Pat, Cmd)] 
           -> Maybe CType
tcArms tenv lenv env t [] = empty -- Can't synthesize type of void elim
tcArms tenv lenv env t [(pat, cmd)] = do
  env' <- tcPat tenv [] pat t
  tcCmd tenv lenv (env' ++ env) cmd
tcArms tenv lenv env t ((pat, cmd) : arms) = do
  env' <- tcPat tenv [] pat t
  tcont <- tcArms tenv lenv env t arms
  tcont' <- tcCmd tenv lenv (env' ++ env) cmd
  if tcont == tcont' then return tcont else empty

-- | Typechecking the fields of a record
tcFields :: TagEnv -> LabEnv -> Env -> [(String, Cmd)] 
             -> Maybe String
tcFields tenv lenv env [] = return "top" -- Name of the empty record
tcFields tenv lenv env [(proj, cmd)] = do
  (a, t) <- lenv proj
  t' <- tcCmd tenv lenv env cmd 
  if t == t' then return a else empty
tcFields tenv lenv env ((proj, cmd) : fields) = do
  (a, t) <- lenv proj
  t' <- tcCmd tenv lenv env cmd
  a' <- tcFields tenv lenv env fields
  if t == t' && a == a' then return a else empty

-- | Typechecking a top-level declaration
tcTop :: TagEnv -> LabEnv -> Env -> TopLevelCmd -> Bool
tcTop tenv lenv env (TopCmd (cmd, t)) = 
  case tcCmd tenv lenv env cmd of
    Just t' -> t == t'
    _ -> False 
tcTop tenv lenv env (TopValue (v, t)) = 
  case tcValue tenv lenv env v of
    Just t' -> t == t'
    _ -> False
tcTop tenv lenv env (TopLet x t v) = 
  case tcValue tenv lenv env v of
    Just t' -> t == t'
    _ -> False
tcTop tenv lenv env (TopDo x t v) = 
  case tcCmd tenv lenv env v of
    Just (V t') -> t == t'
    _ -> False

-- Translating parsed types into internal types

transVT :: Alternative f => Syntax.LType -> f VType
transVT Syntax.VInt        = pure $ VRec "int"
transVT Syntax.VBool       = pure $ VRec "bool"
transVT (Syntax.VForget t) = C <$> transCT t
transVT _                  = empty

transCT :: Alternative f => Syntax.LType -> f CType
transCT (Syntax.CFree t)      = V <$> transVT t
transCT (Syntax.CArrow t1 t2) = CArrow <$> transVT t1 <*> transCT t2
transCT _                     = empty

-- Translating parsed expressions into internal expressions
-- Translation is also intended to serve as a typechecker for the parsed
-- language

primInt :: Env -> Syntax.Expr -> Syntax.Expr -> String -> VType 
            -> Maybe (Value, VType)
primInt env e1 e2 f t = do
  (v1, t1) <- transV env e1
  (v2, t2) <- transV env e2
  case (t1,t2) of
     (VRec "int", VRec "int") ->
       return (Compute ((Force (Var $ Syntax.Name f) `Apply` v1) `Apply` v2),
               t)
     _ -> Nothing

-- | Translate and typecheck a Value
transV :: Env -> Syntax.Expr -> Maybe (Value, VType)
transV env (Syntax.Var x      ) = lookup x env >>= \t -> return (Var x, t)
transV env (Syntax.EInt i     ) = return (Tag (show i) Unit, VRec "int")
transV env (Syntax.EBool True ) = return (Tag "true" Unit, VRec "bool")
transV env (Syntax.EBool False) = return (Tag "true" Unit, VRec "bool")
transV env (Syntax.Times e1 e2) = primInt env e1 e2 "_prim_times" (VRec "int")
transV env (Syntax.Plus e1 e2 ) = primInt env e1 e2 "_prim_plus" (VRec "int")
transV env (Syntax.Minus e1 e2) = primInt env e1 e2 "_prim_minus" (VRec "int")
transV env (Syntax.Equal e1 e2) = primInt env e1 e2 "_prim_eq" (VRec "bool")
transV env (Syntax.Less e1 e2 ) = primInt env e1 e2 "_prim_lt" (VRec "bool")
transV env (Syntax.Thunk e    ) = do
  (cmd, t) <- transC env e
  return (Thunk cmd, C t)
transV env _ = empty

-- | Translate and typecheck a Command
transC :: Env -> Syntax.Expr -> Maybe (Cmd, CType)
transC env (Syntax.Force e) = do
  (v, t) <- transV env e
  case t of
    C t' -> return (Force v, t')
    _ -> Nothing
transC env (Syntax.Return e) = do
  (v, t) <- transV env e
  return (Return v, V t)
transC env (Syntax.To e1 x e2) = do
  (cmd1, t1) <- transC env e1
  case t1 of
    V t1' -> do
      (cmd2, t2) <- transC ((x, t1') : env) e2
      return (Do x cmd1 cmd2, t2)
    _ -> Nothing
transC env (Syntax.Let x e1 e2) = do
  (v1, t1) <- transV env e1
  (cmd2, t2) <- transC ((x, t1) : env) e2
  return (Case v1 [ (PVar x, cmd2) ], t2)
transC env (Syntax.If e1 e2 e3) = do
  (v1, t1) <- transV env e1
  case t1 of
    VRec "bool" -> do
      (cmd2, t2) <- transC env e2
      (cmd3, t3) <- transC env e3
      if t2 /= t3
      then Nothing
      else return (Case v1 [ (PTag "true" PUnit, cmd2),
                             (PTag "false" PUnit, cmd3) ], t2)
    _ -> Nothing
transC env (Syntax.Fun x t1 e) = do
  t1' <- transVT t1
  (cmd, t2) <- transC ((x, t1') : env) e
  return (Fun x t1' cmd, CArrow t1' t2)
transC env (Syntax.Apply e1 e2) = do
  (cmd1, t1) <- transC env e1
  (v2, t2) <- transV env e2
  case t1 of
    CArrow t' t ->
         if t2 /= t'
         then Nothing
         else return (Apply cmd1 v2, t)
    _ -> Nothing
transC env (Syntax.Rec x t e) = do
  t' <- transCT t
  (cmd, t2) <- transC ((x, C t') : env) e
  if t' /= t2
  then Nothing
  else return (Rec x t' cmd, t')
transC env _ = empty

transTop :: Env -> Syntax.TopLevelCmd -> Maybe (TopLevelCmd, Env)
transTop env (Syntax.Expr e) = do
  top <- (TopCmd <$> transC env e) <|> (TopValue <$> transV env e)
  return (top, env)
transTop env (Syntax.Def x e) = do
  (v, t) <- transV env e
  return (TopLet x t v, (x, t) : env)
transTop env (Syntax.RunDef x e) = do
  (cmd, t) <- transC env e
  case t of 
    V t -> return (TopDo x t cmd, (x, t) : env)
    _ -> empty
-- I think these two only really make sense in an interactive top-level
-- loop, and if we have an interactive top-level loop, we just shouldn't
-- have these commands ever get passed on to the translation function. - rjs
transTop env (Syntax.Use s) = empty 
transTop env (Syntax.Quit) = empty 

-- Translate and typecheck as many of the top-level commands as possible
transTops :: Env -> [Syntax.TopLevelCmd] -> [TopLevelCmd]
transTops env [] = []
transTops env (top : tops) = 
  case transTop env top of 
    Nothing -> []
    Just (top', env') -> top' : transTops env' tops
