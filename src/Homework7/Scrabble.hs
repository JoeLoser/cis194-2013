{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Scrabble where

import Data.Char

newtype Score = Score Int
  deriving (Eq, Ord, Show, Num)

score :: Char -> Score
score letter
  | letter' `elem` "aeioulnstr" = Score 1
  | letter' `elem` "dg"         = Score 2
  | letter' `elem` "bcmp"       = Score 3
  | letter' `elem` "fhvwy"      = Score 4
  | letter' `elem` "k"          = Score 5
  | letter' `elem` "jx"         = Score 8
  | letter' `elem` "qz"         = Score 10
  | otherwise                   = Score 0
    where letter' = toLower letter

testScore :: Bool
testScore = and
  [
    score 'A' == 1,
    score 'G' == 2,
    score 'M' == 3,
    score 'W' == 4,
    score 'K' == 5,
    score 'X' == 8,
    score 'Z' == 10,
    score '!' == 0,
    score '.' == 0,
    score '?' == 0
  ]

scoreString :: String -> Score
scoreString = mconcat . fmap score

testScoreString :: Bool
testScoreString = and
  [
    scoreString "add" == 1 + 2 + 2,
    scoreString "hello" == 4 + 1 + 1 + 1 + 1,
    scoreString "Bye" == 3 + 4 + 1
  ]

instance Semigroup Score where
  (<>) = (+)
  {-Score m1 <> Score m2 = Score (m1 + m2)-}

instance Monoid Score where
  mempty  = Score 0
  mappend = (+)
