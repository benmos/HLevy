{-# LANGUAGE FlexibleContexts #-}
--
-- HLevy
-- Copyright (c) 2012 - Ben Moseley
--
module Parser(
  pExpr
)
where

import Syntax

import Control.Applicative

import Text.ParserCombinators.UU
import Text.ParserCombinators.UU.BasicInstances (Str,Parser)

pExpr :: Parser Expr
pExpr = pure $ ConstD 4.7
