{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Risk where

import Control.Monad
import Control.Monad.Random
import Data.List
import Debug.Trace

------------------------------------------------------------
-- Die values

newtype DieValue = DV { unDV :: Int } 
  deriving (Eq, Ord, Show, Num)

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Random DieValue where
  random           = first DV . randomR (1,6)
  randomR (low,hi) = first DV . randomR (max 1 (unDV low), min 6 (unDV hi))

die :: Rand StdGen DieValue
die = getRandom

------------------------------------------------------------
-- Risk

type Army = Int

data Battlefield = Battlefield { attackers :: Army, defenders :: Army } deriving (Show)

-- Exercise 2

battle :: Battlefield -> Rand StdGen Battlefield
battle bf@(Battlefield numAttackers numDefenders)
  | numAttackers < 2 || numDefenders < 1 = return bf
  | otherwise = do
      ds <- replicateM maxDefenders die
      as <- replicateM maxAttackers die
      return $ newBattle (sortDesc as) (sortDesc ds) bf
    where maxAttackers = min 3 (numAttackers - 1)
          maxDefenders = min 2 numDefenders
          sortDesc = sortBy (flip compare)

newBattle :: [DieValue] -> [DieValue] -> Battlefield -> Battlefield
newBattle [] _ = id 
newBattle _ [] = id
newBattle (a:as) (d:ds)
  | d >= a = \bf -> newBattle as ds (defended bf)
  | otherwise = \bf -> newBattle as ds (attacked bf) 
  where
    defended (Battlefield na nd) = Battlefield (na - 1) nd
    attacked (Battlefield na nd) = Battlefield na (nd - 1)

-- Exercise 3

invade :: Battlefield -> Rand StdGen Battlefield
invade bf@(Battlefield 1 _) = return bf
invade bf@(Battlefield _ 0) = return bf
invade bf = battle bf >>= invade

-- Exercise 4
num = 1000
successProb :: Battlefield -> Rand StdGen Double
successProb bf = do
  results <- replicateM num (invade bf)
  return $ fromIntegral (wins results) / fromIntegral num
  where wins = length . filter ((>1) . attackers)
