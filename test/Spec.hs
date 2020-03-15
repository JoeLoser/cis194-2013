import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

import Lib

main :: IO ()

main = hspec $ do
    describe "Exercise 1" $ do
        it "converts 0 to empty list" $ do
            toDigits 0 `shouldBe` ([] :: [Integer])
        it "converts negative number to empty list" $ do
            toDigits (-1) `shouldBe` ([] :: [Integer])
        it "converts an integer into list of digits" $ do
            toDigits 1234 `shouldBe` ([1, 2, 3, 4] :: [Integer])
        it "toDigitsRev converts an integer into list of digits reversed" $ do
            toDigitsRev 1234 `shouldBe` ([4, 3, 2, 1] :: [Integer])
        it "toDigitsRev converts an integer into list of digits reversed on empty list" $ do
            toDigitsRev (-17) `shouldBe` ([] :: [Integer])

    describe "Exercise 2" $ do
        it "doubles every other even list" $ do
            doubleEveryOther [8, 7, 6, 5] `shouldBe` ([16, 7, 12, 5] :: [Integer])
        it "doubles every other odd list" $ do
            doubleEveryOther [1, 2, 3] `shouldBe` ([1, 4, 3] :: [Integer])
