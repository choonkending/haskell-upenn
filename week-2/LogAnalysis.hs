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
