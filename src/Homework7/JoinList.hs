module JoinList where

import Data.Monoid
import Sized

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
      index <- [(-20)..20],
      jl <- [empty, singleA, appendAB, appendABC]
  ]
  where empty     = (Empty :: JoinList Size Char)
        singleA   = Single (Size 1) 'a'
        singleB   = Single (Size 1) 'b'
        singleC   = Single (Size 1) 'c'
        appendAB  = Append (Size 2) singleA singleB
        appendABC = Append (Size 3) appendAB singleC
