module Homework2Spec (spec) where

import Test.Hspec

import LogAnalysis
import Log

spec :: Spec
spec = do
    describe "Exercise 1" $ do
        it "parses error line from log file" $ do
            parseMessage "E 2 562 help help" `shouldBe` (LogMessage (Error 2) 562 "help help" :: LogMessage)
        it "parses warning line from log file" $ do
            parseMessage "W 5 Flange is due for a check-up" `shouldBe` (LogMessage (Warning) 5 "Flange is due for a check-up" :: LogMessage)
        it "parses info line from log file" $ do
            parseMessage "I 29 la la la" `shouldBe` (LogMessage Info 29 "la la la" :: LogMessage)
        it "parses unknown line from log file" $ do
            parseMessage "This is not in the right format" `shouldBe` (Unknown "This is not in the right format" :: LogMessage)
