{-
Name: Miles Goodings
Collaborators: None
Notes: I'm bad at this
-}

module HW01 where

{- Exercise 1 -}

lastDigit :: Integer -> Integer
lastDigit x = x `mod` 10

dropLastDigit :: Integer -> Integer
dropLastDigit x = x `div` 10

{- Exercise 2 -}

toDigits :: Integer -> [Integer]
toDigits x
    | x <= 0 = []                                         -- If x is less than or equal to 0, return empty list
    | otherwise = toDigits (dropLastDigit x) ++ [lastDigit x]

{- Exercise 3 -}

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther x = reverse (doubleEveryOtherRev (reverse x))

doubleEveryOtherRev :: [Integer] -> [Integer]
doubleEveryOtherRev [] = []                                -- If list is empty, return empty list
doubleEveryOtherRev (x:[]) = [x]                           -- If list has single element, return it
doubleEveryOtherRev (x:y:zs) = x : (y * 2) : doubleEveryOtherRev zs

{- Exercise 4 -}

sumDigits :: [Integer] -> Integer
sumDigits [] = 0                                          -- If list is empty, return 0
sumDigits (x:xs) = sumDigitsOne x + sumDigits xs

sumDigitsOne :: Integer -> Integer
sumDigitsOne x
    | x <= 0 = 0                                          -- If x is less than or equal to 0, return 0
    | otherwise = lastDigit x + sumDigitsOne (dropLastDigit x)

{- Exercise 5 -}

validate :: Integer -> Bool
validate x = lastDigit (getChecksum x) == 0

getChecksum :: Integer -> Integer
getChecksum x = sumDigits (doubleEveryOther (toDigits x))

{- Exercise 6 -}

type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi n start end middle
  | n == 0 = []                                        -- If n is 0 return empty list
  | n == 1 = [(start, end)]                            -- If n is one, return Move of start and end
  | otherwise = (hanoi (n - 1) start middle end) ++ [(start, end)] ++ (hanoi (n - 1) middle start end)
