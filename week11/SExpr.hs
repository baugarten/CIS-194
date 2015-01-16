{- CIS 194 HW 11
   due Monday, 8 April
-}

module SExpr where

import AParser
import Control.Applicative
import Data.Char
import Data.Monoid

-- Exercise 1

consA :: Parser a -> Parser [a] -> Parser [a]
consA p1 p2s = (:) <$> p1 <*> p2s

concatA :: Parser [a] -> Parser [a] -> Parser [a]
concatA p1 p2s = (++) <$> p1 <*> p2s

-- But we can actually do this for all Monoids!
combine :: Monoid m => Parser m -> Parser m -> Parser m
combine p1 p2s = (<>) <$> p1 <*> p2s

------------------------------------------------------------
--  1. Parsing repetitions
------------------------------------------------------------

zeroOrMore :: Parser a -> Parser [a]
zeroOrMore p = consA p (zeroOrMore p) <|> pure []

oneOrMore :: Parser a -> Parser [a]
oneOrMore p = consA p (zeroOrMore p)

------------------------------------------------------------
--  2. Utilities
------------------------------------------------------------

spaces :: Parser String
spaces = zeroOrMore (char ' ')

ident :: Parser String
ident = combine (oneOrMore (satisfy isAlpha)) (zeroOrMore (satisfy isAlphaNum))

------------------------------------------------------------
--  3. Parsing S-expressions
------------------------------------------------------------

-- An "identifier" is represented as just a String; however, only
-- those Strings consisting of a letter followed by any number of
-- letters and digits are valid identifiers.
type Ident = String

-- An "atom" is either an integer value or an identifier.
data Atom = N Integer | I Ident
  deriving Show

-- An S-expression is either an atom, or a list of S-expressions.
data SExpr = A Atom
           | Comb [SExpr]
  deriving Show

parseSExpr :: Parser SExpr
parseSExpr = spaces *> either A parseAtom Comb parseList <* spaces 
  where either f1 fa f2 fa2 = (f1 <$> fa) <|> (f2 <$> fa2)
        parseAtom = either N posInt I ident
        parseList = char '(' *> oneOrMore parseSExpr <* char ')'

