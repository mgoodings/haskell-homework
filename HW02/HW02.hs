module HW02 where

import Words
import Data.List

type Hand = [Char]
type Template = String
type STemplate = Template

{- Exercise 1 -}

formableBy :: String -> Hand -> Bool
formableBy [] _ = True
formableBy (w:word) hand
  | elem w hand = formableBy word $ delete w hand
  | otherwise = False

{- Exercise 2 -}

wordsFrom :: Hand -> [String]
wordsFrom hand = filter (`formableBy` hand) allWords

{- Exercise 3 -}

wordFitsTemplate :: Template -> Hand -> String -> Bool
wordFitsTemplate [] _ [] = True
wordFitsTemplate [] _ _ = False
wordFitsTemplate _ _ [] = False
wordFitsTemplate (t:tpl) hand (w:word)
  | t == '?' && formableBy [w] hand = fn
  | t == w = fn
  | otherwise = False
  where fn = (wordFitsTemplate tpl hand word)

{- Exercise 4 -}

wordsFittingTemplate :: Template -> Hand -> [String]
wordsFittingTemplate tpl hand = filter (\word -> wordFitsTemplate tpl hand word) allWords where
  allWords = wordsFrom (tpl ++ hand)

{- Exercise 5 -}

scrabbleValueWord :: String -> Int
scrabbleValueWord [] = 0
scrabbleValueWord word = sum $ map scrabbleValue word

{- Exercise 6 -}

bestWords :: [String] -> [String]
bestWords [] = []
bestWords words = filter (\word -> scrabbleValueWord word == topScore) $ reverse words where
  topScore = maximum $ map scrabbleValueWord words

{- Exercise 7 -}

scrabbleValueTemplate :: STemplate -> String -> Int
scrabbleValueTemplate stpl word = sum (zipWith letterScore stpl scores) * product (map letterMulti stpl) where
  scores = map scrabbleValue word

  letterScore :: Char -> Int -> Int
  letterScore 'D' x = x * 2
  letterScore 'T' x = x * 3
  letterScore _ x = x

  letterMulti :: Char -> Int
  letterMulti '2' = 2
  letterMulti '3' = 3
  letterMulti _ = 1
