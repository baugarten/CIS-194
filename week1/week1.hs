import Data.Char (digitToInt)

-----------------------------------------
-- CC validation

toDigits :: Integer -> [Integer]
toDigits i 
  | i <= 0    = []
  | otherwise = map (toInteger . digitToInt) $ show i

doubleEveryOther :: [Integer] -> [Integer] 
doubleEveryOther xs = reverse $ doubleEveryOther' $ reverse xs
  where doubleEveryOther' (x:x':xs) = x : (2 * x' : (doubleEveryOther' xs))
	doubleEveryOther' xs = xs

sumDigits :: [Integer] -> Integer
sumDigits [x] = x
sumDigits (x:xs) = (sumDigits (toDigits x)) + (sumDigits xs)
sumDigits [] = 0

validate :: Integer -> Bool
validate ccnum = let sum = sumDigits $ doubleEveryOther $ toDigits ccnum
  in sum `mod` 10 == 0


-----------------------------------------
-- Hanoi

type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 1 p1 p2 p3 = [(p1, p2)]
hanoi numDiscs p1 p2 p3 = (hanoi (numDiscs - 1) p1 p3 p2) ++ (hanoi 1 p1 p2 p3) ++ (hanoi (numDiscs - 1) p3 p2 p1)

