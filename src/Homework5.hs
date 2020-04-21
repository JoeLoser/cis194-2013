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


-- Exercise 3
class Expr x where
  lit :: Integer -> x
  add :: x -> x -> x
  mul :: x -> x -> x

instance Expr ExprT where
  lit = ExprT.Lit
  add = ExprT.Add
  mul = ExprT.Mul

-- Given function to constrain the type of its arg to ExprT
reify :: ExprT -> ExprT
reify = id

reifyTest = and
  [
    (reify $ lit 2) == Lit 2,
    (reify $ add (lit 2) (lit 3)) == Add (Lit 2) (Lit 3),
    (reify $ mul (lit 2) (lit 3)) == Mul (Lit 2) (Lit 3)
  ]


-- Exercise 4
instance Expr Integer where
  lit = id
  add = (+)
  mul = (*)

instance Expr Bool where
  lit = (<= 0)
  add = (||)
  mul = (&&)

newtype MinMax = MinMax Integer deriving (Eq, Show)
instance Expr MinMax where
  lit = MinMax
  add (MinMax x) (MinMax y) = MinMax $ max x y
  mul (MinMax x) (MinMax y) = MinMax $ min x y

newtype Mod7 = Mod7 Integer deriving (Eq, Show)
instance Expr Mod7 where
  lit x =  Mod7 $ x `mod` 7
  add (Mod7 x) (Mod7 y) = Mod7 $ (x + y) `mod` 7
  mul (Mod7 x) (Mod7 y) = Mod7 $ (x * y) `mod` 7

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

testInteger = testExp :: Maybe Integer
testBool = testExp :: Maybe Bool
testMinMax = testExp :: Maybe MinMax
testMod7 = testExp :: Maybe Mod7