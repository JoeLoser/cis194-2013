module Golf where

import Data.List

-- Exercise 1
takeEvery :: Integer -> [a] -> [a]
-- Zip to get indices associated with element in list, e.g. [(1, 'A'), (2, 'B'), (3,'C'), (4, 'D')] for "ABCD
-- filter to select nth pairs
-- take each value by mapping over the list of pairs and extracting the value using snd
takeEvery n = map snd . filter (\(x,_) -> x `mod` n == 0) . zip [1..]

skips :: [a] -> [[a]]
skips xs = map (`takeEvery` xs) [1..genericLength xs]

