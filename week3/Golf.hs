module Golf where

import Data.List
import Data.Function (on)


skips :: [a] -> [[a]]
skips xs = map (selectEveryN xs) [1..(length xs)]
  where 
    selectEveryN :: [a] -> Int -> [a]
    selectEveryN xs n = map fst $ filter (\x -> (snd x) `mod` n == 0) (zip xs [1..])

localMaxima :: [Integer] -> [Integer]
localMaxima (x:rest@(y:z:xs))
  | y > x && y > z   = y : (localMaxima rest)
  | otherwise        = localMaxima rest
localMaxima _ = []

-- There should be a way to rotate the histogram with `transpose` but for now
-- this will have to do
histogram :: [Integer] -> String
histogram ints = unlines $ map (showHistRow . (count ints)) [0..9]
  where showHistRow (label, num) = (show label) ++ "| " ++ replicate num '*'

count :: [Integer] -> Integer -> (Integer, Int)
count haystack needle = (needle, length $ filter ((==) needle) haystack)
