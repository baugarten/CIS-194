{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

data ParsedMessageType = Success MessageType [String]
                       | Failure

parseMessage :: String -> LogMessage
parseMessage msg = case parseMessageType (words msg) of
  Success msgType xs -> parseTimestamp msgType xs
  Failure -> Unknown msg
  where
    parseMessageType ("I":xs) = Success Info xs
    parseMessageType ("W":xs) = Success Warning xs
    parseMessageType ("E":num:xs) = Success (Error (read num)) xs
    parseMessageType _ = Failure
    parseTimestamp msgType (num:xs) = LogMessage msgType (read num) (unwords xs)
    parseTimestamp _ _ = Unknown msg

parse :: String -> [LogMessage]
parse file = map parseMessage $ lines file

insert :: LogMessage -> MessageTree -> MessageTree
insert msg@(LogMessage _ _ _) Leaf = Node Leaf msg Leaf
insert msg1@(LogMessage _ t1 _) (Node left msg2@(LogMessage _ t2 _) right) = 
  case t1 < t2 of
    True -> Node (insert msg1 left) msg2 right
    False -> Node left msg2 $ insert msg1 right
insert _ tree = tree
      
build :: [LogMessage] -> MessageTree
build msgs = loop msgs Leaf
  where loop (x:xs) tree = loop xs $ insert x tree
        loop [] tree = tree

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left msg right) = inOrder left ++ [msg] ++ inOrder right

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong msgs = map extractMsg $ filter overFifty (inOrder $ build msgs)
  where overFifty (LogMessage (Error num) _ _) = num > 50
        overFifty _ = False
        extractMsg (LogMessage _ _ msg) = msg
        extractMsg _ = ""
