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
bestWords words = findBestWords words 0 [] where
  findBestWords :: [String] -> Int -> [String] -> [String]
  findBestWords [] _ topWords = topWords
  findBestWords (word:words) bestScore topWords
    | score > bestScore = findBestWords words score [word]
    | score == bestScore = findBestWords words score (word : topWords)
    | otherwise = findBestWords words bestScore topWords
    where score = scrabbleValueWord word

{- Exercise 7 -}

scrabbleValueTemplate :: STemplate -> String -> Int
scrabbleValueTemplate stpl word = scrabbleValueStpl stpl word 0 1 where
  stplScore :: Char -> Int -> Int
  stplScore 'D' x = x * 2
  stplScore 'T' x = x * 3
  stplScore _ x = x

  stplMulti :: Char -> Int -> Int
  stplMulti '2' multi = multi * 2
  stplMulti '3' multi = multi * 3
  stplMulti _ multi = multi

  scrabbleValueStpl :: STemplate -> String -> Int -> Int -> Int
  scrabbleValueStpl [] [] score multi = score * multi
  scrabbleValueStpl (s:stpl) (w:word) score multi =
    scrabbleValueStpl stpl word (score + s `stplScore` value) (s `stplMulti` multi)
    where value = scrabbleValue w
