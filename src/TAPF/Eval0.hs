--
-- Evaluator, variation 0: direct style
--
module TAPF.Eval0 where

import Control.Monad
import Control.Monad.Identity

data Expr = Const Int
          | Add Expr Expr
          | Mul Expr Expr
          | Div Expr Expr
          | Neg Expr
           deriving Show

-- | evaluation monad  
type Eval a = Maybe a

eval :: Expr -> Eval Int
eval (Const n) = return n

eval (Add e1 e2) = 
     do v1 <- eval e1
        v2 <- eval e2
        return (v1+v2)
     

eval (Mul e1 e2) = do
     v1<-eval e1
     v2<-eval e2
     return (v1*v2)

eval (Div e1 e2) = do v1 <- eval e1
                      v2 <- eval e2
                      if v2 == 0 then Nothing
                      else
                         return (v1`div`v2)

eval (Neg e) = do v <- eval e; return (negate v)



-- examples
ex1 = Mul (Const 3) (Add (Const 1) (Const 2))
ex2 = Div (Const 1) (Const 0)
