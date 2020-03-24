module GolfSpec (spec) where

import Test.Hspec

import Golf

spec :: Spec
spec = do
    describe "Exercise 1" $ do
        it "skips even length string" $ do
            skips "ABCD" `shouldBe` (["ABCD", "BD", "C", "D"] :: [String])
        it "skips odd length string" $ do
            skips "hello" `shouldBe` (["hello", "el", "l", "l", "o"] :: [String])
        it "skips list of one element" $ do
            skips [1] `shouldBe` ([[1]] :: [[Int]])
        it "skips list of two elements" $ do
            skips [True, False] `shouldBe` ([[True, False], [False]] :: [[Bool]])

    describe "Exercise 2" $ do
        it "single local maxima" $ do
            localMaxima [2, 3, 4, 1, 5] `shouldBe` ([4] :: [Integer])
        it "two local maxima" $ do
            localMaxima [2, 9, 5, 6, 1] `shouldBe` ([9, 6] :: [Integer])
        it "no local maxima" $ do
            localMaxima [1..5] `shouldBe` ([] :: [Integer])
