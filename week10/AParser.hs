{- CIS 194 HW 10
   due Monday, 1 April
-}

module AParser where

import           Control.Applicative

import           Data.Char

-- A parser for a value of type a is a function which takes a String
-- represnting the input to be parsed, and succeeds or fails; if it
-- succeeds, it returns the parsed value along with the remainder of
-- the input.
newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

-- For example, 'satisfy' takes a predicate on Char, and constructs a
-- parser which succeeds only if it sees a Char that satisfies the
-- predicate (which it then returns).  If it encounters a Char that
-- does not satisfy the predicate (or an empty input), it fails.
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
  where
    f [] = Nothing    -- fail on the empty input
    f (x:xs)          -- check if x satisfies the predicate
                        -- if so, return x along with the remainder
                        -- of the input (that is, xs)
        | p x       = Just (x, xs)
        | otherwise = Nothing  -- otherwise, fail

-- Using satisfy, we can define the parser 'char c' which expects to
-- see exactly the character c, and fails otherwise.
char :: Char -> Parser Char
char c = satisfy (== c)

{- For example:

*Parser> runParser (satisfy isUpper) "ABC"
Just ('A',"BC")
*Parser> runParser (satisfy isUpper) "abc"
Nothing
*Parser> runParser (char 'x') "xyz"
Just ('x',"yz")

-}

-- For convenience, we've also provided a parser for positive
-- integers.
posInt :: Parser Integer
posInt = Parser f
  where
    f xs
      | null ns   = Nothing
      | otherwise = Just (read ns, rest)
      where (ns, rest) = span isDigit xs

------------------------------------------------------------
-- Your code goes below here
------------------------------------------------------------
  
first :: (a -> b) -> (a, c) -> (b, c)
first f ac = (f $ fst ac, snd ac)

instance Functor Parser where
  fmap f parser = Parser parse
    where parse s = first f <$> runParser parser s

instance Applicative Parser where
  pure a = Parser (\s -> Just (a, s))

  p1 <*> p2 = Parser parse
    where parse s = do
          r1 <- runParser p1 s
          r2 <- runParser p2 (snd r1)
          return (fst r1 $ fst r2, snd r2)

instance Alternative Parser where
  empty = Parser $ const Nothing
  p1 <|> p2= Parser parse
    where parse s = let r1 = runParser p1 s
                        r2 = runParser p2 s 
                    in r1 <|> r2

-- Exercise 5

nullParser :: Parser a -> Parser ()
nullParser p = const () <$> p


-- type Name = String
-- data Employee = Emp { name :: Name, phone :: String }

-- parseName :: Parser Name
-- parseName = satisfy True 
-- 
-- parsePhone :: Parser String
-- parsePhone = satisfy isDigit

-- Exercise 3

aParser :: Parser (Char, Char)
aParser = (\x y -> (x, y)) <$> char 'a' <*> char 'b'

aParser_ :: Parser ()
aParser_ = (\x y -> ()) <$> char 'a' <*> char 'b'

space :: Parser Char
space = char ' '

intPair :: Parser [Integer]
intPair = (\x s y -> [x, y]) <$> posInt <*> space <*> posInt


-- Exercise 5

nullParser :: Parser a -> Parser ()
nullParser p = const () <$> p

intOrUppercase :: Parser ()
intOrUppercase = nullParser posInt <|> nullParser (satisfy isUpper)
