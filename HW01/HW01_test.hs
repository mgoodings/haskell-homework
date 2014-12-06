module HW01_test where

import HW01
import Test.Hspec

spec :: Spec
spec = do
  describe "Exercise 1" $ do
    context "lastDigit" $ do
      it "lastDigit 123 == 3" $ do
        lastDigit 123 `shouldBe` 3

      it "lastDigit 0 == 0" $ do
        lastDigit 0 `shouldBe` 0

    context "dropLastDigit" $ do
      it "dropLastDigit 123 == 12" $ do
        dropLastDigit 123 `shouldBe` 12

      it "dropLastDigit 5 == 0" $ do
        dropLastDigit 5 `shouldBe` 0

  describe "Exercise 2" $ do
    context "toDigits" $ do
      it "toDigits 1234 == [1,2,3,4]" $ do
        toDigits 1234 `shouldBe` [1,2,3,4]

      it "toDigits 0 == []" $ do
        toDigits 0 `shouldBe` []

      it "toDigits (-17) == []" $ do
        toDigits (-17) `shouldBe` []

  describe "Exercise 3" $ do
    context "doubleEveryOther" $ do
      it "doubleEveryOther [8,7,6,5] == [16,7,12,5]" $ do
        doubleEveryOther [8,7,6,5] `shouldBe` [16,7,12,5]

      it "doubleEveryOther [1,2,3] == [1,4,3]" $ do
        doubleEveryOther [1,2,3] `shouldBe` [1,4,3]

  describe "Exercise 4" $ do
    context "sumDigits" $ do
      it "sumDigits [16,7,12,5] == 22" $ do
        sumDigits [16,7,12,5] `shouldBe` 22

  describe "Exercise 5" $ do
    context "validate" $ do
      it "validate 4012888888881881 == True" $ do
        validate 4012888888881881 `shouldBe` True

      it "validate 4012888888881882 == True" $ do
        validate 4012888888881882 `shouldBe` False

  describe "Exercise 6" $ do
    context "hanoi" $ do
      it "hanoi 2 \"a\" \"b\" \"c\" == [(\"a\",\"c\"), (\"a\",\"b\"), (\"c\",\"b\")]" $ do
        hanoi 2 "a" "b" "c" `shouldBe` [("a","c"), ("a","b"), ("c","b")]

  describe "Exercise 7" $ do
    context "hanoi4" $ do
      it "length (hanoi4 15 \"a\" \"b\" \"c\" \"d\") == 129" $ do
        length (hanoi4 15 "a" "b" "c" "d") `shouldBe` 129

main :: IO ()
main = hspec spec
