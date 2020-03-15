{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module LogAnalysis where

import Log

-- Exercise 1
parseMessage :: String -> LogMessage
parseMessage x = case words x of
  "E":severity:timestamp:msg -> LogMessage(Error (read severity)) (read timestamp) (unwords msg)
  "W":timestamp:msg -> LogMessage Warning (read timestamp) (unwords msg)
  "I":timestamp:msg -> LogMessage Info (read timestamp) (unwords msg)
  msg -> Unknown(unwords msg)

parse :: String -> [LogMessage]
parse = map parseMessage . lines

-- Exercise 2
insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert msg Leaf = Node Leaf msg Leaf
insert msg1@(LogMessage _ ts1 _) (Node left msg2 @(LogMessage _ ts2 _) right)
  | ts1 < ts2 = Node (insert msg1 left) msg2 right
  | otherwise = Node left msg2 (insert msg1 right)

-- Exercise 3
build :: [LogMessage] -> MessageTree
build = foldr insert Leaf

-- Exercise 4
inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left msg right) = inOrder left ++ [msg] ++ inOrder right

-- Exercise 5
isSevere :: Int -> LogMessage -> Bool
isSevere minLevel (LogMessage (Error level) _ _)
  | level > minLevel = True
  | otherwise = False
isSevere _ _ = False

extractMsg :: [LogMessage] -> [String]
extractMsg (LogMessage _ _ msg : msgs) = msg : extractMsg msgs
extractMsg _ = []

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = extractMsg . inOrder . build . filter(isSevere 50)
