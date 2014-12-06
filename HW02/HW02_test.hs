module HW02_test where

import HW02
import Test.Hspec

spec :: Spec
spec = do
  describe "Exercise 1" $ do
    context "formableBy" $ do
      it "formableBy \"fun\" ['x','n','i','f','u','e','l'] == True" $ do
        formableBy "fun" ['x','n','i','f','u','e','l'] `shouldBe` True

      it "formableBy \"haskell\" ['k','l','e','h','a','l','s'] == True" $ do
        formableBy "haskell" ['k','l','e','h','a','l','s'] `shouldBe` True

      it "formableBy \"haskell\" ['k','l','e','h','a','y','s'] == False" $ do
        formableBy "haskell" ['k','l','e','h','a','y','s'] `shouldBe` False

  describe "Exercise 2" $ do
    context "wordsFrom" $ do
      it "wordsFrom ['a','b','c','d'] == [\"ab\",\"ad\",\"ba\",\"bad\",\"cab\",\"cad\",\"dab\"]" $ do
        wordsFrom ['a','b','c','d'] `shouldBe` [
          "ab","ad","ba","bad","cab","cad","dab"]

      it "wordsFrom ['h','e','l','l','o'] == [\"eh\",\"el\",\"ell\",\"he\",\"hell\",\"hello\",\"helo\",\"ho\",\"hoe\",\"hole\",\"lo\",\"oe\",\"oh\",\"ole\"]" $ do
        wordsFrom ['h','e','l','l','o'] `shouldBe` [
          "eh","el","ell","he","hell","hello","helo","ho","hoe","hole","lo","oe","oh","ole"]

  describe "Exercise 3" $ do
    context "wordFitsTemplate" $ do
      it "wordFitsTemplate \"??r?\" ['c','x','e','a','b','c','l'] \"care\" == True" $ do
        wordFitsTemplate "??r?" ['c','x','e','a','b','c','l'] "care" `shouldBe` True

      it "wordFitsTemplate \"??r?\" ['c','x','e','w','b','c','l'] \"care\" == False" $ do
        wordFitsTemplate "??r?" ['c','x','e','w','b','c','l'] "care" `shouldBe` False

      it "wordFitsTemplate \"??r?\" ['c','x','e','a','b','c','l'] \"car\" == False" $ do
        wordFitsTemplate "??r?" ['c','x','e','a','b','c','l'] "car" `shouldBe` False

      it "wordFitsTemplate \"let\" ['x','x'] \"let\" == True" $ do
        wordFitsTemplate "let" ['x','x'] "let" `shouldBe` True

  describe "Exercise 4" $ do
    context "wordsFittingTemplate" $ do
      it "wordsFittingTemplate \"??r?\" ['c','x','e','a','b','c','l'] == [\"acre\",\"bare\",\"carb\",\"care\",\"carl\",\"earl\"]" $ do
        wordsFittingTemplate "??r?" ['c','x','e','a','b','c','l'] `shouldBe` [
          "acre","bare","carb","care","carl","earl"]

  describe "Exercise 5" $ do
    context "scrabbleValueWord" $ do
      it "scrabbleValueWord \"care\" == 6" $ do
        scrabbleValueWord "care" `shouldBe` 6

      it "scrabbleValueWord \"quiz\" == 22" $ do
        scrabbleValueWord "quiz" `shouldBe` 22

  describe "Exercise 6" $ do
    context "bestWords" $ do
      it "bestWords (wordsFittingTemplate \"??r?\" ['c','x','e','a','b','c','l']) == [\"carb\"]" $ do
        bestWords (wordsFittingTemplate "??r?" ['c','x','e','a','b','c','l']) `shouldBe` ["carb"]

      it "bestWords [\"cat\", \"rat\", \"bat\"] == [\"bat\",\"cat\"]" $ do
        bestWords ["cat", "rat", "bat"] `shouldBe` ["bat","cat"]

      it "bestWords [] == []" $ do
        bestWords [] `shouldBe` []

  describe "Exercise 7" $ do
    context "scrabbleValueTemplate" $ do
      it "scrabbleValueTemplate \"?e??3\" \"peace\" == 27" $ do
        scrabbleValueTemplate "?e??3" "peace" `shouldBe` 27

      it "scrabbleValueTemplate \"De?2?\" \"peace\" == 24" $ do
        scrabbleValueTemplate "De?2?" "peace" `shouldBe` 24

      it "scrabbleValueTemplate \"??Tce\" \"peace\" == 11" $ do
        scrabbleValueTemplate "??Tce" "peace" `shouldBe` 11

main :: IO ()
main = hspec spec
