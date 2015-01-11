{-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-# LANGUAGE FlexibleInstances #-}

module Fibonacci where

import qualified Data.Map as Map

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

fibs1 :: [Integer]
fibs1 = map fib [1..]

fibs2 ::[Integer]
fibs2 = map fibs2Calc [0..]
  where fibs2Calc 0 = 0
        fibs2Calc 1 = 1
        fibs2Calc n' = fibs2 !! (n'-1) + fibs2 !! (n'-2)

data Stream a = Cons a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Cons a stream) = a : streamToList stream

instance Show a => Show (Stream a) where
  show stream = unwords (map show $ take 20 $ streamToList stream)

streamRepeat :: a -> Stream a
streamRepeat a = Cons a $ streamRepeat a

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons x xs) = Cons (f x) $ streamMap f xs

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f a = Cons a $ streamFromSeed f (f a)

nats :: Stream Integer
nats = streamFromSeed (+1) 1

-- 0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0, 4, . . .
ruler :: Stream Integer
ruler = streamMap f $ streamFromSeed (+1) 1
  where f x | odd x = 0
            | otherwise = 1 + f (x `div` 2)

zeros :: Stream Integer
zeros = streamRepeat 0

x :: Stream Integer
x = Cons 0 $ Cons 1 zeros

instance Num (Stream Integer) where
  fromInteger i = Cons i zeros
  negate (Cons x xs) = Cons (-x) $ negate xs
  (+) (Cons x xs) (Cons y ys) = Cons (x+y) (xs + ys)
  (*) (Cons x xs) s2@(Cons y ys) = Cons (x*y) ((streamMap (*x) ys) + (xs * s2))

instance Fractional (Stream Integer) where
  (/) s1@(Cons x xs) s2@(Cons y ys) = Cons (x `div` y) $ streamMap (`div` y) (xs - (s1 / s2) * ys)

fibs3 = (Cons 0 $ Cons 1 $ zeros) / (Cons 1 $ Cons (-1) $ Cons (-1) zeros)
