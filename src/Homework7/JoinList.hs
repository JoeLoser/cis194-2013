module JoinList where

import Data.Monoid
import Sized
import Scrabble

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
  deriving (Eq, Show)

-- Exercise 1
(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
jl1 +++ jl2 = Append (tag jl1 <> tag jl2) jl1 jl2

-- Get annotation at the root of a JoinList
tag :: Monoid m => JoinList m a -> m
tag Empty = mempty
tag (Single m _) = m
tag (Append m _ _) = m

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
      tag Empty == "",
      tag (Single "a" 'a') == "a",
      tag (Append "ab" (Single "a" 'a') (Single "b" 'b')) == "ab"
  ]


-- Exercise 2
getTaggedSize :: (Monoid b, Sized b) => JoinList b a -> Int
getTaggedSize = getSize . size . tag

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
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


dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
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


takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
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
