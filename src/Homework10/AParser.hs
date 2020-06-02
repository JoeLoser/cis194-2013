{-{-# OPTIONS_GHC -Wall #-}-}

module AParser where

import Control.Applicative
import Data.Char

-- A parser for a value of type a is a function which takes a String
-- represnting the input to be parsed, and succeeds or fails; if it
-- succeeds, it returns the parsed value along with the remainder of
-- the input.
newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

-- For example, 'satisfy' takes a predicate on Char, and constructs a
-- parser which succeeds only if it sees a Char that satisfies the
-- predicate (which it then returns).  If it encounters a Char that
-- does not satisfy the predicate (or an empty input), it fails.
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
  where
    f [] = Nothing    -- fail on the empty input
    f (x:xs)          -- check if x satisfies the predicate
                        -- if so, return x along with the remainder
                        -- of the input (that is, xs)
        | p x       = Just (x, xs)
        | otherwise = Nothing  -- otherwise, fail

-- Using satisfy, we can define the parser 'char c' which expects to
-- see exactly the character c, and fails otherwise.
char :: Char -> Parser Char
char c = satisfy (== c)

{- For example:

*Parser> runParser (satisfy isUpper) "ABC"
Just ('A',"BC")
*Parser> runParser (satisfy isUpper) "abc"
Nothing
*Parser> runParser (char 'x') "xyz"
Just ('x',"yz")

-}

-- For convenience, we've also provided a parser for positive
-- integers.
posInt :: Parser Integer
posInt = Parser f
  where
    f xs
      | null ns   = Nothing
      | otherwise = Just (read ns, rest)
      where (ns, rest) = span isDigit xs

-- Exercise 1
-- Apply a given function to first element of pair
first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Functor Parser where
  fmap f p = Parser $ fmap (first f) . runParser p

-- Exercise 2
instance Applicative Parser where
  pure a = Parser f
    where f str = Just (a, str)

  p1 <*> p2 = Parser p
    where p str = runParser p1 str >>= g -- Run p1 to produce function which is passed to g
          g (f, remainingInput) = runParser (f <$> p2) remainingInput
                                {-= runParser fmappedParser remainingInput-}

-- Exercise 3
abParser :: Parser (Char, Char)
abParser = (,) <$> char 'a' <*> char 'b'

abParserTest = and
  [
    runParser abParser "abcdef" == Just (('a', 'b'), "cdef"),
    runParser abParser "notab" == Nothing
  ]

-- Just like abParser above but returns () instead of characters 'a' and 'b'
abParser_ :: Parser ()
abParser_ = const () <$> abParser

abParser_Test = and
  [
    runParser abParser_ "abcdef" == Just ((), "cdef"),
    runParser abParser_ "notab" == Nothing
  ]

-- Parser for two integers separated by a space
intPair :: Parser [Integer]
intPair = (\a _ b -> [a, b]) <$> posInt <*> char ' ' <*> posInt

intPairTest = and
  [
    runParser intPair "12 34" == Just ([12, 34], ""),
    runParser intPair "1234" == Nothing,
    runParser intPair "12 34 56" == Nothing
  ]
