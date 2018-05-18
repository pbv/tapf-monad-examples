--
-- Evaluator, variation 0: direct style
--
module Eval0 where

import Control.Monad

data Expr = Const Int
          | Add Expr Expr
          | Mul Expr Expr
          | Div Expr Expr
          | Neg Expr
           deriving Show

-- | evaluation monad: just optional results 
type Eval a = Maybe a

eval :: Expr -> Eval Int
eval (Const n) = return n

eval (Add e1 e2) = 
  do v1 <- eval e1
     v2 <- eval e2
     return (v1+v2)
  
eval (Mul e1 e2) =
  do v1 <- eval e1
     v2 <- eval e2
     return (v1*v2)
  
eval (Div e1 e2) = 
  do v1 <- eval e1
     v2 <- eval e2
     guard (v2 /= 0)
     return (v1`div`v2)
  
eval (Neg e) = 
  do v <- eval e
     return (negate v)


-- examples
ex1 = Mul (Const 3) (Add (Const 1) (Const 2))
ex2 = Div (Const 1) (Const 0)


