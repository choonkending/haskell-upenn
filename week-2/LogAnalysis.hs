{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

convertStringToInt :: String -> Int
convertStringToInt s = read s :: Int

parseMessage :: String -> LogMessage
parseMessage s
  | h == "I" = LogMessage Info (convertStringToInt (head t)) (unwords (tail t))
  | h == "W" = LogMessage Warning (convertStringToInt (head t)) (unwords (tail t))
  | h == "E" = LogMessage (Error (convertStringToInt (head t))) (convertStringToInt (head (drop 1 t))) (unwords (drop 2 t))
  | otherwise = Unknown s
  where
    l = words s
    h = head l
    t = tail l

parse :: String -> [LogMessage]
parse = map parseMessage . lines
