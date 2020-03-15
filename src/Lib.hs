module Lib
( someFunc, toDigits, toDigitsRev, doubleEveryOther
) where

-- Exercise 1
toDigits :: Integer -> [Integer]
toDigits x
    | x <= 0 = []
    | x < 10 = [x]
    | otherwise = toDigits (x `div` 10) ++ [x `mod` 10]

toDigitsRev :: Integer -> [Integer]
toDigitsRev x = reverse (toDigits x)

-- Exercise 2
doubleEveryOtherLeft [] = []
doubleEveryOtherLeft (x:[]) = [x]
doubleEveryOtherLeft (x:y:zs) = x : 2 * y : doubleEveryOtherLeft zs

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther xs = reverse (doubleEveryOtherLeft(reverse xs))

someFunc :: IO ()
someFunc = putStrLn "someFunc"
