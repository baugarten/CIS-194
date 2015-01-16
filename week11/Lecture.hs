module Lecture where

import Control.Applicative

(*>) :: Applicative f => f a -> f b -> f b
fa *> fb = fb

mapA :: Applicative f => (a -> f b) -> ([a] -> f [b])
mapA f = \lst -> case lst of
  [] -> pure []
  (a:as) -> (\x y -> x ++ y) <$> (pure <$> f a) <*> (mapA f as)

sequenceA :: Applicative f => [f a] -> f [a]
sequenceA [] = pure []
sequenceA (fa:fas) = (\x y -> x ++ y) <$> (pure <$> fa) <*> (sequenceA fas)

replicateA :: Applicative f => Int -> f a -> f [a]
replicateA n f = sequenceA (replicate n f)

