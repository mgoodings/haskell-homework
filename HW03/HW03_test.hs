module HW03_test where

import Log
import LogAnalysis
import Test.Hspec
import System.IO.Unsafe

readLogMessages :: FilePath -> [LogMessage]
readLogMessages file = parse (unsafePerformIO (readFile file))

spec :: Spec
spec = do
  describe "Exercise 1" $ do
    context "parseMessage" $ do
      it "parseMessage \"E 2 562 help help\" == ValidLM (LogMessage (Error 2) 562 \"help help\")" $ do
        parseMessage "E 2 562 help help" `shouldBe` ValidLM (LogMessage (Error 2) 562 "help help")

      it "parseMessage \"I 29 la la la\" == ValidLM (LogMessage Info 29 \"la la la\")" $ do
        parseMessage "I 29 la la la" `shouldBe` ValidLM (LogMessage Info 29 "la la la")

      it "parseMessage \"This is not in the right format\" == InvalidLM \"This is not in the right format\"" $ do
        parseMessage "This is not in the right format" `shouldBe` InvalidLM "This is not in the right format"

  describe "Exercise 2" $ do
    context "validMessagesOnly" $ do
      it "validMessagesOnly [\
          \ValidLM (LogMessage Info 1 \"abc\"), \
          \InvalidLM \"abc\", \
          \ValidLM (LogMessage Warning 2 \"def\")\
        \] == [\
          \LogMessage Info 1 \"abc\", \
          \LogMessage Warning 2 \"def\"\
        \]" $ do
        validMessagesOnly [
            ValidLM (LogMessage Info 1 "abc"),
            InvalidLM "abc",
            ValidLM (LogMessage Warning 2 "def")
          ] `shouldBe` [
            LogMessage Info 1 "abc",
            LogMessage Warning 2 "def"
          ]

  describe "Exercise 3" $ do
    context "parse" $ do
      it "head (parse (\"sample.log\")) == (LogMessage Info 6 \"Completed armadillo processing\")" $ do
        head (readLogMessages "sample.log") `shouldBe` (LogMessage Info 6 "Completed armadillo processing")

      it "length (parse (\"sample.log\")) == 11" $ do
        length (readLogMessages "sample.log") `shouldBe` 11

  describe "Exercise 4" $ do
    context "compareMsgs" $ do
      it "compareMsgs \
        \(LogMessage Warning 153 \"Not a speck of light is showing, so the danger must be growing...\") \
        \(LogMessage Info 208 \"the Weighted Companion Cube cannot talk\") == LT" $ do
        compareMsgs
          (LogMessage Warning 153 "Not a speck of light is showing, so the danger must be growing...")
          (LogMessage Info 208 "the Weighted Companion Cube cannot talk")
        `shouldBe` LT

      it "compareMsgs \
        \(LogMessage (Error 101) 2001 \"My God! It’s full of stars!\") \
        \(LogMessage Info 2001 \"Daisy, Daisy, give me your answer do.\") == EQ" $ do
        compareMsgs
          (LogMessage (Error 101) 2001 "My God! It’s full of stars!")
          (LogMessage Info 2001 "Daisy, Daisy, give me your answer do.")
        `shouldBe` EQ

  describe "Exercise 5" $ do
    context "sortMessages" $ do
      it "head (sortMessages (\"sample.log\")) == (LogMessage Info 1 \"Nothing to report\")" $ do
        head (sortMessages (readLogMessages "sample.log")) `shouldBe` (LogMessage Info 1 "Nothing to report")

      it "last (sortMessages (\"sample.log\")) == (LogMessage Info 11 \"Initiating self-destruct sequence\")" $ do
        last (sortMessages (readLogMessages "sample.log")) `shouldBe` (LogMessage Info 11 "Initiating self-destruct sequence")

  describe "Exercise 6" $ do
    context "whatWentWrong" $ do
      it "whatWentWrong (\"sample.log\") == [\
          \\"Way too many pickles\",\
          \\"Bad pickle-flange interaction detected\",\
          \\"Flange failed!\"\
        \]" $ do
        whatWentWrong (readLogMessages "sample.log") `shouldBe` [
            "Way too many pickles",
            "Bad pickle-flange interaction detected",
            "Flange failed!"
          ]

  describe "Exercise 7" $ do
    context "messagesAbout" $ do
      it "messagesAbout \"relish\" [\
          \LogMessage Info 2001 \"Relishsign detected!!\", \
          \LogMessage Info 3001 \"This is not the message you are looking for.\"\
        \] == [\
          \LogMessage Info 2001 \"Relishsign detected!!\"\
        \]" $ do
        messagesAbout "relish" [
            LogMessage Info 2001 "Relishsign detected!!",
            LogMessage Info 3001 "This is not the message you are looking for."
          ] `shouldBe` [
            LogMessage Info 2001 "Relishsign detected!!"
          ]

      it "messagesAbout \"relish\" [LogMessage Info 2001 \"I see nothing!!\"] == []" $ do
        messagesAbout "relish" [LogMessage Info 2001 "I see nothing!!"] `shouldBe` []

  describe "Exercise 8" $ do
    context "whatWentWrongEnhanced" $ do
      it "whatWentWrongEnhanced \"pickle\" (\"sample.log\") == [\
        \\"Way too many pickles\",\
        \\"Bad pickle-flange interaction detected\"\
      \]" $ do
        whatWentWrongEnhanced "pickle" (readLogMessages "sample.log") `shouldBe` [
            "Way too many pickles",
            "Bad pickle-flange interaction detected"
          ]

      it "whatWentWrongEnhanced \"garbage\" (\"sample.log\") == []" $ do
        whatWentWrongEnhanced "garbage" (readLogMessages "sample.log") `shouldBe` []

main :: IO ()
main = hspec spec
