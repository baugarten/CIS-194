{-# LANGUAGE TypeSynonymInstances #-}

module Calc where

import Expr
import ExprT
import Parser

eval :: ExprT -> Integer
eval (Lit int) = int
eval (Add exp1 exp2) = (eval exp1) + (eval exp2)
eval (Mul exp1 exp2) = (eval exp1) * (eval exp2)

evalStr :: String -> Maybe Integer
evalStr str = fmap eval (parseExp Lit Add Mul str)

instance Expr ExprT where
  lit = Lit
  add = Add
  mul = Mul

instance Expr Integer where
  lit = id
  add = (+)
  mul = (*)

instance Expr Bool where
  lit n | n <=0 = False
        | otherwise = True
  add = (||)
  mul = (&&)

newtype MinMax = MinMax Integer deriving (Eq, Show)

instance Expr MinMax where
  lit = MinMax
  add (MinMax x) (MinMax y) = MinMax $ max x y
  mul (MinMax x) (MinMax y) = MinMax $ min x y

newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr Mod7 where
  lit n = Mod7 $ n `mod` 7
  add (Mod7 a) (Mod7 b) = Mod7 $ (a + b) `mod` 7
  mul (Mod7 a) (Mod7 b) = Mod7 $ (a * b) `mod` 7

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

testInteger = testExp :: Maybe Integer
testBool = testExp :: Maybe Bool
testMM = testExp :: Maybe MinMax
testSat = testExp :: Maybe Mod7

