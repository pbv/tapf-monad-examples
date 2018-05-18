--
-- Evaluator, variation 2: local variables
--

module Eval2 where

import Control.Monad.Reader

type Name = String

data Expr = Const Int
          | Var Name
          | Add Expr Expr
          | Mul Expr Expr
          | Div Expr Expr
          | Neg Expr
          | Let Name Expr Expr   -- "let x = e1 in e2"
           deriving Show

type Env = [(Name, Int)]

-- | evaluation monad
type Eval a = Reader Env a

-- | evaluator 
eval :: Expr -> Eval Int
eval (Const n)  = return n

eval (Var x)  = do
  env <- ask
  case lookup x env of
    Just v -> return v
    Nothing -> error "unbound variable"
    
eval (Add e1 e2)  = do
  v1 <- eval e1 
  v2 <- eval e2 
  return (v1+v2)

eval (Mul e1 e2)  = do
  v1 <- eval e1 
  v2 <- eval e2 
  return (v1*v2)

eval (Neg e)  = do
  v <- eval e 
  return (negate v)

eval (Div e1 e2) = do
  v1 <- eval e1 
  v2 <- eval e2 
  return (v1 `div` v2) -- no error check!

eval (Let x e1 e2)= do
  v1 <- eval e1
  local (\env -> (x,v1):env) $ eval e2

-- examples
ex1 = Mul (Const 23) (Add (Const 1) (Const 2))
ex2 = Div (Const 1) (Const 0)

ex3 = (Let "x"
        (Add (Const 1) (Const 2))
        (Mul (Const 3) (Var "x"))
      )

ex4 = Mul
      (Let "x"
       (Const 1)
       (Add (Const 1) (Var "x"))
      )
      (Var "x")
