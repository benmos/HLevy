module IL (
  -- * Types
  VType(..),
  CType(..),
  Pat(..),
  Value(..),
  Cmd(..),
  Env,

  -- * Functions
  transV,
  transC
)
where

import Control.Applicative
import qualified Syntax

data VType = VRec String
           | VPair VType VType
           | VUnit
           | C CType
             deriving (Eq,Ord,Show)

data CType = CRec String
           | CArrow VType CType
           | V VType
             deriving (Eq,Ord,Show)

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

data Cmd   = Print Value                -- print "abc"
           | Do Syntax.Name Cmd Cmd     -- do x <- cmd cmd
           | Case Value [ (Pat, Cmd) ]  -- case v of | p1 -> cmd1 | pn -> cmdn
           | Fun Syntax.Name VType Cmd  -- fn x:t -> cmd
           | Rec Syntax.Name CType Cmd  -- rec x:t is cmd
           | Force Value                -- force v
           | Return Value               -- return v
           | Apply Cmd Value            -- apply cmd v
           | Project Cmd String         -- cmd.foo
           | Record [ (String, Cmd) ]   -- { l1=cmd1, ..., ln=cmdn }
             deriving (Eq,Ord,Show)

type Env = [ (Syntax.Name, VType) ]


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

primInt :: Env -> Syntax.Expr -> Syntax.Expr -> String -> VType -> Maybe (Value, VType)
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
  return (Fun x t1' cmd, t2)
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