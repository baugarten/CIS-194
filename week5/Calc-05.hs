{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Calc where

import Expr
import Parser
import StackVM

instance Expr Program where
  lit x = [PushI x]
  add x y = x ++ y ++ [Add]
  mul x y = x ++ y ++ [Mul]

compile :: String -> Maybe Program
compile = parseExp lit add mul
