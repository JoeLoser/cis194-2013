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

