module Homework6 where

-- Exercise 1
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib(n-2)

testFib = and
  [
    fib 0 == 0,
    fib 1 == 1,
    fib 2 == 1,
    fib 3 == 2,
    fib 4 == 3,
    fib 5 == 5,
    fib 6 == 8,
    fib 7 == 13,
    fib 8 == 21
  ]


fibs1 :: [Integer]
fibs1 = map fib [0..]

-- Exercise 2
fibs2' :: Integer -> Integer -> [Integer]
fibs2' x y = x : fibs2' y (x + y)

fibs2 :: [Integer]
fibs2 = fibs2' 0 1

testFibs2 = take 20 fibs1 == take 20 fibs2

-- Exercise 3
data Stream element = Cons element (Stream element)

instance Show a => Show (Stream a) where
    show = show . take 20 . streamToList

streamToList :: Stream a -> [a]
streamToList (Cons x y) = x : streamToList y

-- Exercise 4
streamRepeat :: a -> Stream a
streamRepeat x = Cons x (streamRepeat x)

instance Functor Stream where
    fmap f (Cons x xs) = Cons (f x) (fmap f xs)

streamMap :: (a -> b) -> Stream a -> Stream b
{-Can define inline as below, or define fmap on Stream-}
{-streamMap f (Cons x xs) = Cons (f x) (streamMap f xs)-}
streamMap = fmap

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f seed = Cons seed (streamFromSeed f (f seed))

-- Exercise 5
nats :: Stream Integer
nats = streamFromSeed (+1) 0
