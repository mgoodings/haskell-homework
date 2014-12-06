module HW01 where

lastDigit :: Integer -> Integer
lastDigit x = x `mod` 10

dropLastDigit :: Integer -> Integer
dropLastDigit x = x `div` 10

toDigits :: Integer -> [Integer]
toDigits x
  | x <= 0 = []
  | otherwise = toDigits (dropLastDigit x) ++ [lastDigit x]

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther x = reverse (doubleEveryOther' (reverse x)) where
  doubleEveryOther' :: [Integer] -> [Integer]
  doubleEveryOther' [] = []
  doubleEveryOther' (x:[]) = [x]
  doubleEveryOther' (x:y:zs) = x : (y * 2) : doubleEveryOther' zs

sumDigits :: [Integer] -> Integer
sumDigits xs = sum (concatMap toDigits xs)

validate :: Integer -> Bool
validate x = lastDigit (getChecksum x) == 0 where
  getChecksum :: Integer -> Integer
  getChecksum x = sumDigits (doubleEveryOther (toDigits x))

type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi 1 p1 p2 _ = [(p1, p2)]
hanoi n p1 p2 p3 =
  hanoi (n - 1) p1 p3 p2 ++ [(p1, p2)] ++ hanoi (n - 1) p3 p2 p1

hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4 0 _ _ _ _ = []
hanoi4 1 p1 p2 _ _ = [(p1, p2)]
hanoi4 n p1 p2 p3 r =
  hanoi4 k p1 p3 p2 r ++ hanoi (n - k) p1 p2 r ++ hanoi4 k p3 p2 p1 r
  where k = n `div` 2
