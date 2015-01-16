module AParser (Parser, runParser, satisfy, char, posInt) where

import           Control.Applicative
import           Data.Char
import           Data.Monoid
-- import Lecture

newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
  where
    f [] = Nothing
    f (x:xs)
        | p x       = Just (x, xs)
        | otherwise = Nothing

char :: Char -> Parser Char
char c = satisfy (== c)

posInt :: Parser Integer
posInt = Parser f
  where
    f xs
      | null ns   = Nothing
      | otherwise = Just (read ns, rest)
      where (ns, rest) = span isDigit xs

inParser f = Parser . f . runParser

first :: (a -> b) -> (a,c) -> (b,c)
first f (x,y) = (f x, y)

instance Functor Parser where
  fmap = inParser . fmap . fmap . first

instance Applicative Parser where
  pure a = Parser (\s -> Just (a, s))
  (Parser fp) <*> xp = Parser $ \s ->
    case fp s of
      Nothing     -> Nothing
      Just (f,s') -> runParser (f <$> xp) s'

instance Alternative Parser where
  empty = Parser (const Nothing)
  Parser p1 <|> Parser p2 = Parser $ liftA2 (<|>) p1 p2

-- Exercise 1

consA :: Parser a -> Parser [a] -> Parser [a]
consA p1 p2s = (:) <$> p1 <*> p2s

concatA :: Parser [a] -> Parser [a] -> Parser [a]
concatA p1 p2s = (++) <$> p1 <*> p2s

-- But we can actually do this for all Monoids!
combine :: Monoid m => Parser m -> Parser m -> Parser m
combine p1 p2s = (<>) <$> p1 <*> p2s

zeroOrMore :: Parser a -> Parser [a]
zeroOrMore p = consA p (zeroOrMore p) <|> pure []

oneOrMore :: Parser a -> Parser [a]
oneOrMore p = consA p (zeroOrMore p)

-- Exercise 2

spaces :: Parser String
spaces = zeroOrMore (char ' ')

ident :: Parser String
ident = combine (oneOrMore (satisfy isAlpha)) (zeroOrMore (satisfy isAlphaNum))

-- Exercise 3

-- type Ident = String
-- 
-- data Atom = N Integer | I Ident
--   deriving Show
-- 
-- data SExpr = A Atom
--            | Comb [SExpr]
--   deriving Show
