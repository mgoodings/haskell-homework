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
  | w `elem` hand = formableBy word (w `delete` hand)
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
  | t == '?' && (formableBy [w] hand) = wordFitsTemplate tpl hand word
  | t == w = wordFitsTemplate tpl hand word
  | otherwise = False

{- Exercise 4 -}

wordsFittingTemplate :: Template -> Hand -> [String]
wordsFittingTemplate tpl hand = filter (\word -> wordFitsTemplate tpl hand word) allWords where
  allWords = wordsFrom (tpl ++ hand)

{- Exercise 5 -}

scrabbleValueWord :: String -> Int
scrabbleValueWord [] = 0
scrabbleValueWord (w:word) = scrabbleValue w + scrabbleValueWord word

{- Exercise 6 -}

bestWords :: [String] -> [String]
bestWords [] = []
bestWords words = bestWordsAcc 0 [] words where
  bestWordsAcc :: Int -> [String] -> [String] -> [String]
  bestWordsAcc _ topWords [] = topWords
  bestWordsAcc best topWords (word:words)
    | score > best = bestWordsAcc score [word] words
    | score == best = bestWordsAcc score (word : topWords) words
    | otherwise = bestWordsAcc best topWords words
    where score = scrabbleValueWord word

{- Exercise 7 -}

scrabbleValueTemplate :: STemplate -> String -> Int
scrabbleValueTemplate stpl word = scrabbleValueStpl stpl word 0 1 where
  stplScore :: Char -> Char -> Int
  stplScore s w
    | s == 'D' = value * 2
    | s == 'T' = value * 3
    | otherwise = value
    where value = scrabbleValue w

  stplMultiplier :: Char -> Int -> Int
  stplMultiplier s multiplier
    | s == '2' = multiplier * 2
    | s == '3' = multiplier * 3
    | otherwise = multiplier

  scrabbleValueStpl :: STemplate -> String -> Int -> Int -> Int
  scrabbleValueStpl [] [] score multiplier = score * multiplier
  scrabbleValueStpl (s:stpl) (w:word) score multiplier =
    scrabbleValueStpl stpl word (score + (s `stplScore` w)) (s `stplMultiplier` multiplier)
