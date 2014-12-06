module LogAnalysis where

import Log
import Data.List(sortBy, isInfixOf)
import Data.Char(toLower)

{- Exercise 1 -}

parseMessage :: String -> MaybeLogMessage
parseMessage s = case ws of
  ("I":t:m)   -> ValidLM $ LogMessage Info (read t :: TimeStamp) $ unwords m
  ("W":t:m)   -> ValidLM $ LogMessage Warning (read t :: TimeStamp) $ unwords m
  ("E":c:t:m) -> ValidLM $ LogMessage (Error $ read c) (read t :: TimeStamp) $ unwords m
  _           -> InvalidLM s
  where ws = words s

{- Exercise 2 -}

validMessagesOnly :: [MaybeLogMessage] -> [LogMessage]
validMessagesOnly [] = []
validMessagesOnly ((ValidLM x):xs) = x : validMessagesOnly xs
validMessagesOnly (_:xs) = validMessagesOnly xs

{- Exercise 3 -}

parse :: String -> [LogMessage]
parse n = validMessagesOnly (map parseMessage (lines n))

{- Exercise 4 -}

compareMsgs :: LogMessage -> LogMessage -> Ordering
compareMsgs (LogMessage _ t1 _) (LogMessage _ t2 _)
  | (t1 > t2) = GT
  | (t1 < t2) = LT
  | (t1 == t2) = EQ

{- Exercise 5 -}

sortMessages :: [LogMessage] -> [LogMessage]
sortMessages msgs = sortBy compareMsgs msgs

{- Exercise 6 -}

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong msgs = whatWentWrong' (sortMessages msgs) where
  whatWentWrong' :: [LogMessage] -> [String]
  whatWentWrong' [] = []
  whatWentWrong' ((LogMessage (Error e) _ msg):xs)
    | e >= 50 = msg : whatWentWrong' xs
    | otherwise = whatWentWrong' xs
  whatWentWrong' (_:xs) = whatWentWrong' xs

{- Exercise 7 -}

messagesAbout :: String -> [LogMessage] -> [LogMessage]
messagesAbout _ [] = []
messagesAbout match (l@(LogMessage _ _ msg):logs)
  | matchString match msg = l : messagesAbout match logs
  | otherwise = messagesAbout match logs
  where
    matchString :: String -> String -> Bool
    matchString needle haystack = (map toLower needle) `isInfixOf` (map toLower haystack)

{- Exercise 8 -}

whatWentWrongEnhanced :: String -> [LogMessage] -> [String]
whatWentWrongEnhanced match msgs = whatWentWrong (messagesAbout match msgs)
