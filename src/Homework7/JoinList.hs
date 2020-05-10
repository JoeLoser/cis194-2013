module JoinList where

import Data.Monoid

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
    empty +++ singleLetter == Append empty "a" singleLetter,
    singleLetter +++ empty == Append "a" empty singleLetter,
    empty +++ appendTwoLetter == Append empty "ab" appendTwoLetter,
    appendTwoLetter +++ empty = Append "ab" appendTwoLetter empty,
    singleLetter +++ anotherSingleLetter == Append "ab" singleLetter anotherSingleLetter,
    doubleLetter +++ singleLetter == Append "abc" doubleLetter singleLetter,
    singleLetter +++ doubleLetter == Append "abc" singleLetter doubleLetter,
    doubleLetter +++ anotherDoubleLetter == Append "abcd" doubleLetter anotherDoubleLetter
  ]
  where empty == (Empty :: JoinList String Char)
        singleLetter == Single "a" 'a'
        anotherSingleLetter == Single "b" 'b'
        singleC == Single "c" 'c'
        singleD == Single "d" 'd'
        doubleLetter == Append "ab" singleLetter anotherSingleLetter
        anotherDoubleLetter == Append "cd" singleC singleD


tagTest :: Bool
tagTest = and
  [
      tag Empty == "",
      tag (Single "a" 'a') == "a",
      tag (Append "ab" (Single "a" 'a') (Single "b" 'b')) == "ab"
  ]
