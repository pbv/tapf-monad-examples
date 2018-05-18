--
-- Monadic Evaluator, variation 1
-- Exceptions
--

module Eval1 where

data Expr = Const Int
          | Add Expr Expr
          | Mul Expr Expr
          | Div Expr Expr
          | Neg Expr
           deriving Show

-- | evaluation monad
type Eval a = Either String a

eval :: Expr -> Eval Int
eval (Const n) = return n

eval (Add e1 e2) = do
  v1 <- eval e1
  v2 <- eval e2
  return (v1+v2)

eval (Mul e1 e2) = do
  v1 <- eval e1
  v2 <- eval e2
  return (v1*v2)

eval (Neg e) = do
  v <- eval e
  return (negate v)

eval (Div e1 e2)= do
  v1 <- eval e1
  v2 <- eval e2
  if v2 /= 0 then
    return (v1 `div` v2)
    else
    throwError ("division by zero: " ++ show (Div e1 e2)) 

throwError :: e -> Either e a
throwError = Left


-- examples
ex1 = Mul (Const 23) (Add (Const 1) (Const 2))
ex2 = Div (Const 1) (Const 0)

ex3 = (Add
       (Div (Const 2) (Const 0))
       (Div (Const 3) (Const 0))
      )
