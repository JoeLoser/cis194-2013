module Homework5 where

import ExprT
import Parser

-- Exercise 1
eval :: ExprT -> Integer
eval (Lit x) = x
eval (Add e1 e2) = eval e1 + eval e2
eval (Mul e1 e2) = eval e1 * eval e2


evalTest = and
  [
  eval (Lit 2) == 2,
  eval (Add (Lit 2) (Lit 3)) == 5,
  eval (Mul (Add (Lit 2) (Lit 3)) (Lit 4)) == 20
  ]


-- Exercise 2
evalStr :: String -> Maybe Integer
evalStr = fmap eval . parseExp Lit Add Mul

evalStrTest = and
  [
  evalStr "42" == Just 42,
  evalStr "3 + 5" == Just 8,
  evalStr "3 * 2" == Just 6,
  evalStr "3 + * 2" == Nothing
  ]
