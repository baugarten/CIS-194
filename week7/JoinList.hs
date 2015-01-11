{-# LANGUAGE FlexibleInstances #-}
module JoinList where

import Buffer
import Data.Monoid
import Editor
import Scrabble
import Sized

data JoinList m a = Empty
     	          | Single m a
     	          | Append m (JoinList m a) (JoinList m a)
                  deriving (Eq, Show)

instance Monoid m => Monoid (JoinList m a) where
  mempty = Empty
  mappend = (+++)

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a 
list1 +++ list2 = Append (tag list1 <> tag list2) list1 list2

tag :: Monoid m => JoinList m a -> m
tag Empty = mempty
tag (Single m _) = m
tag (Append m _ _) = m

sz :: (Monoid b, Sized b) => JoinList b a -> Int
sz = getSize . size . tag

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty = Nothing
indexJ n _ | n < 0 = Nothing
indexJ n jl | n > sz jl = Nothing
indexJ n (Single _ v) = Just v
indexJ n (Append m ljl rjl)
  | n < sz ljl = indexJ n ljl
  | otherwise = indexJ (n - sz ljl) rjl

jlToList :: JoinList m a -> [a]
jlToList Empty = []
jlToList (Single _ a) = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ _ Empty = Empty
dropJ n jl | n > sz jl = Empty
dropJ n jl | n <= 0 = jl
dropJ n s@(Single _ _) = Empty
dropJ n (Append m ljl rjl)
  | n < sz ljl = dropJ n ljl +++ rjl
  | otherwise  = dropJ (n - sz ljl) rjl

jlmappend :: Monoid m => JoinList m a -> JoinList m a -> m
jlmappend jl1 jl2 = tag jl1 <> tag jl2

takeJ :: (Monoid b, Sized b) => Int -> JoinList b a -> JoinList b a
takeJ _ Empty = Empty
takeJ n jl | n > sz jl = jl
takeJ n jl | n <= 0 = Empty
takeJ n s@(Single _ _) = s
takeJ n (Append m ljl rjl)
  | n < sz ljl = takeJ n ljl
  | otherwise  = ljl +++ (takeJ (n - sz ljl) rjl)

scoreLine :: String -> JoinList Score String
scoreLine str = Single (scoreString str) str


------------------------------------------------------
-- Exercise 4

instance Buffer (JoinList (Score, Size) String) where
  toString = unwords . jlToList
  fromString = mconcat . map (\l -> Single (scoreString l, Size 1) l) . lines
  --fromString = foldr (+++) Empty . map (\s -> Single (scoreString s, Size 1) s) . lines
  
  line = indexJ
  
  replaceLine n str jl = takeJ n jl +++ fromString str +++ dropJ (n+1) jl

  numLines = sz

  value = getScore . fst . tag

main = runEditor editor jlbuffer 
  where jlbuffer = fromString $ unlines
         [ "This buffer is for notes you don't want to save, and for"
         , "evaluation of steam valve coefficients."
         , "To load a different file, type the character L followed"
         , "by the name of the file."
         ] :: (JoinList (Score, Size) String)
