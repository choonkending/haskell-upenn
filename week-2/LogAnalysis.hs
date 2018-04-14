{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

convertStringToInt :: String -> Int
convertStringToInt s = read s :: Int

-- parseMessage :: String -> LogMessage
-- parseMessage s
--   | h == "I" = LogMessage Info (convertStringToInt (head t)) (unwords (tail t))
--   | h == "W" = LogMessage Warning (convertStringToInt (head t)) (unwords (tail t))
--   | h == "E" = LogMessage (Error (convertStringToInt (head t))) (convertStringToInt (head (drop 1 t))) (unwords (drop 2 t))
--   | otherwise = Unknown s
--   where
--     l = words s
--     h = head l
--     t = tail l
-- parseMessage s
--   | h1 == "I" = LogMessage Info (convertStringToInt h2) (unwords t)
--   | h1 == "W" = LogMessage Warning (convertStringToInt h2) (unwords t)
--   | h1 == "E" = LogMessage (Error (convertStringToInt h2)) (convertStringToInt (head t)) (unwords (drop 1 t))
--   | otherwise = Unknown s
--   where
--     (h1 : h2 : t) = words s

parseWords :: [String] -> LogMessage
parseWords ("I" : h : t) = LogMessage Info (convertStringToInt h) (unwords t)
parseWords ("W" : h : t) = LogMessage Warning (convertStringToInt h) (unwords t)
parseWords ("E" : h1 : h2: t) = LogMessage (Error (convertStringToInt h1)) (convertStringToInt h2) (unwords t)
parseWords s = Unknown (unwords s)

parseMessage :: String -> LogMessage
parseMessage = parseWords . words

parse :: String -> [LogMessage]
parse = map parseMessage . lines

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) m = m
insert l@(LogMessage _ _ _) Leaf = Node Leaf l Leaf
-- insert l1@(LogMessage _ t1 _)
--   (Node leftTree l2@(LogMessage _ t2 _) rightTree) =
--     if t1 < t2
--       then Node (insert l1 leftTree) l2 rightTree
--       else Node leftTree l2 (insert l1 rightTree)

insert l1@(LogMessage _ t1 _)
  (Node leftTree l2@(LogMessage _ t2 _) rightTree)
  | t1 < t2 = Node (insert l2 leftTree) l2 rightTree
  | otherwise = Node leftTree l2 (insert l1 rightTree)

