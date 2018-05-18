--
-- Evaluator, variation 3: state
--

module Eval3 where

import           Data.Map (Map)
import qualified Data.Map as Map
import           Control.Monad.State

-- | variable names 
type Name = String

-- | expressions 
data Expr = Const Int
          | Var Name
          | Add Expr Expr
          | Mul Expr Expr
          | Div Expr Expr
          | Neg Expr
           deriving Show

-- | statements
data Stmt = Assign Name Expr
          | Seq Stmt Stmt
           deriving Show

-- | store mappings names to values
type Store = Map Name Int


-- | evaluation monad
type Eval a = State Store a

-- | run function for our Eval monad;
-- in this case it just runState 
runEval :: Eval a -> Store -> (a, Store)
runEval = runState

-- | evaluator for expressions
evalE :: Expr -> Eval Int
evalE (Const n)  = return n

evalE (Var x)  = do
  env <- get
  case Map.lookup x env of
    Just v -> return v
    Nothing -> error "unbound variable"
    
evalE (Add e1 e2)  = do
  v1 <- evalE e1 
  v2 <- evalE e2 
  return (v1+v2)

evalE (Mul e1 e2)  = do
  v1 <- evalE e1 
  v2 <- evalE e2 
  return (v1*v2)

evalE (Neg e)  = do
  v <- evalE e 
  return (negate v)

evalE (Div e1 e2) = do
  v1 <- evalE e1 
  v2 <- evalE e2 
  return (v1 `div` v2) -- no error check!


-- | evaluator for statements;
-- result is the last assigned value
evalS :: Stmt -> Eval Int
evalS (Assign x e) = do
  v <- evalE e
  modify (Map.insert x v)
  return v

evalS (Seq c1 c2) = do
  evalS c1
  evalS c2


-------------------------------------------------------------
-- examples
-------------------------------------------------------------

ex1 = Mul (Const 23) (Add (Const 1) (Const 2))
ex2 = Div (Const 1) (Const 0)

ex3 = Seq (Assign "x" (Const 1))
          (Assign "x" (Add (Const 1) (Var "x")))

ex4 = Seq (Assign "x" (Const 1))
          (Assign "x" (Div (Const 1) (Const 0)))
