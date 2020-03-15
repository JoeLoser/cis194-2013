module Lib
( someFunc, toDigits, toDigitsRev, doubleEveryOther, sumDigits, validate
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

-- Exercise 3
sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:xs) = sum (toDigits x) + sumDigits xs

-- Exercise 4
checksum :: Integer -> Integer
checksum x = sumDigits(doubleEveryOther (toDigits x)) `mod` 10

validate :: Integer -> Bool
validate x = checksum x == 0

someFunc :: IO ()
someFunc = putStrLn "someFunc"
