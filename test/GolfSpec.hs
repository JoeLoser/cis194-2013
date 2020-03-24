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
