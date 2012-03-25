{-# LANGUAGE FlexibleContexts, GADTs #-}
--
-- HLevy
-- Copyright (c) 2012 - Ben Moseley
--
module Syntax(
  LType(..),
  Expr(..),
  Name(..)
)
where

import Control.Applicative

newtype Name = Name { unName :: String } deriving (Eq,Ord)

instance Show Name where show (Name s) = s

data LType = VInt
           | VBool
           | VForget CType
           | CFree VType
           | CArrow VType CType
             deriving (Eq,Ord,Show)

type CType = LType
type VType = LType

type Value = Expr

data Expr = Var    Name
          | EInt   Int
          | EBool  Bool
          | Times  Value Value
          | Plus   Value Value
          | Minus  Value Value
          | Equal  Value Value
          | Less   Value Value
          | Thunk  Expr
          | Force  Value
          | Return Value
          | To     Expr  Name  Expr
          | Let    Name  Value Expr
          | If     Value Expr  Expr
          | Fun    Name  LType Expr
          | Apply  Expr  Value
          | Rec    Name  LType Expr
             deriving (Eq,Ord,Show)

data TopLevelCmd = Expr Expr
                 | Def Name Expr
                 | RunDef Name Expr
                 | Use String
                 | Quit
