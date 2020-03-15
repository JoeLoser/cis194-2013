module Homework1Spec (spec) where

import Test.Hspec

import Homework1

spec :: Spec
spec = do
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

    describe "Exercise 3" $ do
        it "calculate sum of digits" $ do
            sumDigits [16, 7, 12, 5] `shouldBe` (22 :: Integer)

    describe "Exercise 4" $ do
        it "validates valid number" $ do
            validate 4012888888881881 `shouldBe` True
        it "Validates invalid number" $ do
            validate 4012888888881882 `shouldBe` False

    describe "Exercise 5" $ do
        it "2 disks" $ do
            hanoi 2 "a" "b" "c" `shouldBe` [("a", "c"), ("a", "b"), ("c", "b")]
