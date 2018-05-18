--
-- Evaluator, variation 5
-- logging
--
module Eval5 where

import Control.Monad.Writer

data Expr = Const Int
          | Add Expr Expr
          | Mul Expr Expr
          | Div Expr Expr
          | Neg Expr
           deriving Show

type Eval a = Writer String a

eval :: Expr -> Eval Int
eval (Const n) = return n

eval (Add e1 e2) = do
  v1 <- eval e1
  v2 <- eval e2
  logger $ show v1 ++ "+" ++ show v2 ++ "=" ++ show (v1+v2) 
  return (v1+v2)

eval (Mul e1 e2) = do
  v1 <- eval e1
  v2 <- eval e2
  logger $ show v1 ++ "*" ++ show v2 ++ "=" ++ show (v1*v2)
  return (v1*v2)
  
eval (Div e1 e2) = do
  v1 <- eval e1
  v2 <- eval e2
  logger $ show v1 ++ "div" ++ show v2 ++ "=" ++ show (v1`div`v2)
  return (v1`div`v2)
  
eval (Neg e) = do
  v <- eval e
  return (negate v)

-- | log a single text line
logger :: String -> Eval ()
logger line = tell (line ++ "\n")


-- examples
ex1 = Mul (Const 23) (Add (Const 1) (Const 2))
ex2 = Div (Const 1) (Const 0)

ex3 = Mul (Add (Const 1) (Const 2)) (Add (Const 3) (Const 4))
