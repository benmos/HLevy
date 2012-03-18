{-# LANGUAGE FlexibleContexts #-}
--
-- HLevy
-- Copyright (c) 2012 - Ben Moseley
--
module Syntax(
  Expr(..)
)
where

import Control.Applicative

data Expr =
    Add Expr Expr
  | Mul Expr Expr
  | Sub Expr Expr
  | ConstD Double

