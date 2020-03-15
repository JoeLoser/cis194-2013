module Lib
( someFunc, toDigits, toDigitsRev
) where

-- Exercise 1
toDigits :: Integer -> [Integer]
toDigits x
    | x <= 0 = []
    | x < 10 = [x]
    | otherwise = toDigits (x `div` 10) ++ [x `mod` 10]

toDigitsRev :: Integer -> [Integer]
toDigitsRev x = reverse (toDigits x)

someFunc :: IO ()
someFunc = putStrLn "someFunc"
