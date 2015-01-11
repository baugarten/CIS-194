{-# LANGUAGE FlexibleInstances #-}

module Calc where

import Control.Monad
import Expr
import Parser
import qualified Data.Map as M

data VarExprT = Lit Integer
              | Add VarExprT VarExprT
              | Mul VarExprT VarExprT
              | Var String
              deriving (Show, Eq)

class HasVars a where
  var :: String -> a

instance Expr VarExprT where
  lit = Lit
  add = Add
  mul = Mul

instance HasVars VarExprT where
  var = Var

instance HasVars (M.Map String Integer -> Maybe Integer) where
  var = M.lookup

instance Expr (M.Map String Integer -> Maybe Integer) where
  lit n m = Just n
  add x y m = liftM2 (+) (x m) (y m)
  mul x y m = liftM2 (*) (x m) (y m)

withVars :: [(String, Integer)] 
         -> (M.Map String Integer 
         -> Maybe Integer) -> Maybe Integer
withVars vs exp = exp $ M.fromList vs
