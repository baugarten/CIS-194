module Week4 where

import Data.Bool
import Data.List
import Debug.Trace

fun1 :: [Integer] -> Integer
fun1 x = product (filter even x)

fun2 :: Integer -> Integer
fun2 n = sum $ filter even $ takeWhile (/= 1) $ iterate step n

step n | even n = n `div` 2
       | otherwise = 3 * n + 1

data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
            deriving (Show, Eq)

foldTree :: [a] -> Tree a
foldTree = foldr insertTree Leaf

insertTree :: a -> Tree a -> Tree a
insertTree elem Leaf = Node 0 Leaf elem Leaf
insertTree insertElem (Node depth l elem r) 
  | hl < hr = Node hl (insertTree insertElem l) elem r
  | hl > hr = Node hr l elem r2
  | otherwise = Node (hr2+1) l elem r2
  where
    hl = heightTree l
    hr = heightTree r
    r2 = insertTree insertElem r
    hr2 = heightTree r2

heightTree :: Tree a -> Integer
heightTree Leaf = 0
heightTree (Node depth _ _ _) = depth



xor :: [Bool] -> Bool
xor [True,False] = True
xor [False,True] = True
xor [x,y] = False
xor xs = foldr (\b1 b2 -> xor [b1, b2]) False xs

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\a acc -> f a : acc) []

sieveSanduram :: Integer -> [Integer]
sieveSanduram n = [2*x+1 | x <- [1..n], notElem x deleted]
  where deleted = sundDelete n

sundDelete :: Integer -> [Integer]
sundDelete n = [i+j+2*i*j | j <- [1..n], i <- [1..j]]

