{-# LANGUAGE FlexibleInstances #-}

module JoinList where

import Data.Monoid
import Data.Semigroup

import Buffer
import Sized
import Scrabble

data JoinList s a = Empty
                  | Single s a
                  | Append s (JoinList s a) (JoinList s a)
  deriving (Eq, Show)

-- Exercise 1
(+++) :: Semigroup s => JoinList s a -> JoinList s a -> JoinList s a
jl1 +++ jl2 = Append (tag jl1 <> tag jl2) jl1 jl2

-- Get annotation at the root of a JoinList
tag :: Semigroup s => JoinList s a -> s
tag Empty = undefined
tag (Single s _) = s
tag (Append s _ _) = s

appendTest :: Bool
appendTest = and
  [
    empty +++ empty == Append mempty empty empty,
    empty +++ singleLetter == Append "a" empty singleLetter,
    singleLetter +++ empty == Append "a" empty singleLetter,
    empty +++ doubleLetter == Append "ab" empty doubleLetter,
    doubleLetter +++ empty == Append "ab" doubleLetter empty,
    singleLetter +++ anotherSingleLetter == Append "ab" singleLetter anotherSingleLetter,
    doubleLetter +++ singleLetter == Append "abc" doubleLetter singleLetter,
    singleLetter +++ doubleLetter == Append "abc" singleLetter doubleLetter,
    doubleLetter +++ anotherDoubleLetter == Append "abcd" doubleLetter anotherDoubleLetter
  ]
  where empty = (Empty :: JoinList String Char)
        singleLetter = Single "a" 'a'
        anotherSingleLetter = Single "b" 'b'
        singleC = Single "c" 'c'
        singleD = Single "d" 'd'
        doubleLetter = Append "ab" singleLetter anotherSingleLetter
        anotherDoubleLetter = Append "cd" singleC singleD


tagTest :: Bool
tagTest = and
  [
      {-tag Empty == "",-}
      tag (Single "a" 'a') == "a",
      tag (Append "ab" (Single "a" 'a') (Single "b" 'b')) == "ab"
  ]


-- Exercise 2
getTaggedSize :: (Semigroup b, Sized b) => JoinList b a -> Int
getTaggedSize = getSize . size . tag

indexJ :: (Sized b, Semigroup b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty                               = Nothing
indexJ index _ | index < 0                   = Nothing
indexJ index jl | index >= getTaggedSize jl  = Nothing
indexJ _ (Single _ a)                        = Just a
indexJ index (Append _ jl1 jl2)
  | index < leftSize                         = indexJ index jl1
  | otherwise                                = indexJ (index - leftSize) jl2
  where leftSize = getTaggedSize jl1


-- Safe list indexing function provided
(!!?) :: [a] -> Int -> Maybe a
[] !!? _        = Nothing
_ !!? i | i < 0 = Nothing
(x:_) !!? 0     = Just x
(_:xs) !!? i    = xs !!? (i-1)

-- Convert a JoinList into a list ignoring monoidal annotations
jlToList :: JoinList m a -> [a]
jlToList Empty            = []
jlToList (Single _ a)     = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2

indexJTest :: Bool
indexJTest = and
  [
    jlToList jl !!? index == indexJ index jl |
      index <- [(-10)..10],
      jl <- [empty, singleA, appendAB, appendABC]
  ]
  where empty     = (Empty :: JoinList Size Char)
        singleA   = Single (Size 1) 'a'
        singleB   = Single (Size 1) 'b'
        singleC   = Single (Size 1) 'c'
        appendAB  = Append (Size 2) singleA singleB
        appendABC = Append (Size 3) appendAB singleC


dropJ :: (Sized b, Semigroup b) => Int -> JoinList b a -> JoinList b a
dropJ _ Empty                               = Empty
dropJ index jl | index <= 0                 = jl
dropJ index jl | index >= getTaggedSize jl  = jl
dropJ _ (Single _ _)                        = Empty
dropJ n (Append _ jl1 jl2)
    | n < leftSize                          = dropJ n jl1 +++ jl2
    | otherwise                             = dropJ (n - leftSize) jl2
  where leftSize = getTaggedSize jl1


dropJTest :: Bool
dropJTest = and
  [
    jlToList (dropJ n jl) == drop n (jlToList jl) |
      n <- [(-10)..10],
      jl <- [empty, singleA, appendAB, appendABC]
  ]
  where empty     = (Empty :: JoinList Size Char)
        singleA   = Single (Size 1) 'a'
        singleB   = Single (Size 1) 'b'
        singleC   = Single (Size 1) 'c'
        appendAB  = Append (Size 2) singleA singleB
        appendABC = Append (Size 3) appendAB singleC


takeJ :: (Sized b, Semigroup b) => Int -> JoinList b a -> JoinList b a
takeJ _ Empty                      = Empty
takeJ n _  | n <= 0                = Empty
takeJ n jl | n >= getTaggedSize jl = jl
takeJ _ (Single b a)               = Single b a
takeJ n (Append _ jl1 jl2)
    | n < leftSize                 = takeJ n jl1
    | otherwise                    = jl1 +++ takeJ (n - leftSize) jl2
    where leftSize = getTaggedSize jl1

takeJTest :: Bool
takeJTest = and
  [
    jlToList (takeJ n jl) == take n (jlToList jl) |
      n <- [(-10)..10],
      jl <- [empty, singleA, appendAB, appendABC]
  ]
  where empty     = (Empty :: JoinList Size Char)
        singleA   = Single (Size 1) 'a'
        singleB   = Single (Size 1) 'b'
        singleC   = Single (Size 1) 'c'
        appendAB  = Append (Size 2) singleA singleB
        appendABC = Append (Size 3) appendAB singleC

-- Exercise 3
-- Returns a JoinList with the score of the input string as the metadata
scoreLine :: String -> JoinList Score String
scoreLine s = Single (scoreString s) s

testScoreLine :: Bool
testScoreLine = and
  [
    scoreLine "yay" == Single (Score 9) "yay",
    scoreLine "haskell" == Single (Score 14) "haskell",
    scoreLine "yay " +++ scoreLine "haskell!" ==
      Append (Score 23) (Single (Score 9) "yay ") (Single (Score 14) "haskell!")
  ]


-- Exercise 4
instance Semigroup m => Semigroup (JoinList m a) where
  (<>) = (+++)

instance Monoid m => Monoid (JoinList m a) where
  mempty = Empty

instance Buffer (JoinList (Score, Size) String) where
  toString Empty = ""
  toString (Single _ s) = s
  toString (Append _ jl1 jl2) = toString jl1 ++ "\n" ++ toString jl2

  {--- | Create a buffer from a String.-}
  {-fromString :: String -> b-}
  fromString = undefined

  {--- | Extract the nth line (0-indexed) from a buffer.  Return Nothing-}
  {--- for out-of-bounds indices.-}
  line = indexJ

  {---   with the @n@th line replaced by @ln@.  If the index is-}
  {---   out-of-bounds, the buffer should be returned unmodified.-}
  {-replaceLine :: Int -> String -> b -> b-}
  replaceLine n s jl = takeJ n jl +++ fromString s +++ dropJ (n + 1) jl

  {--- | Compute the number of lines in the buffer.-}
  numLines Empty = 0
  numLines (Single (_, size) _) = getSize size
  numLines (Append (_, size) _ _) = getSize size

  {--- | Compute the value of the buffer, i.e. the amount someone would-}
  {---   be paid for publishing the contents of the buffer.-}
  {-value :: b -> Int-}
  value = undefined
