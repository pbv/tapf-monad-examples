--
-- Evaluator, variation 3
-- state and exceptions combined
--

module Eval4 where

import           Data.Map (Map)
import qualified Data.Map as Map

import           Control.Monad.State
import           Control.Monad.Except

type Name = String

data Expr = Const Int
          | Var Name
          | Add Expr Expr
          | Mul Expr Expr
          | Div Expr Expr
          | Neg Expr
           deriving Show

data Stmt = Assign Name Expr
          | Seq Stmt Stmt
          deriving Show

type Store = Map Name Int


-- the evaluation monad
-- combing state and exceptions
-- stacking monads this way we preserve state after an exception
type Eval a = ExceptT String (State Store) a

runEval :: Eval a -> Store -> (Either String a, Store)
runEval m s0 = runState (runExceptT m) s0


{-
-- stacking monads this way state is lost after an exception

type Eval a = StateT Store (Either String) a
runEval :: Eval a -> Store -> Either String (a, Store)
runEval m s0 = runStateT m s0
-}

-- | evaluator for expressions
evalE :: Expr -> Eval Int
evalE (Const n) = return n

evalE (Var x) = do
  store <- get
  case Map.lookup x store of
    Just v -> return v
    Nothing -> throwError ("unbound variable" ++ show x)

evalE (Add e1 e2) = do
  v1 <- evalE e1 
  v2 <- evalE e2 
  return (v1+v2)

evalE (Mul e1 e2) = do
  v1 <- evalE e1 
  v2 <- evalE e2 
  return (v1*v2)

evalE (Neg e)  = do
  v <- evalE e 
  return (negate v)

evalE (Div e1 e2)  = do
  v1 <- evalE e1 
  v2 <- evalE e2 
  if v2 /= 0 then
    return (v1 `div` v2)
    else
    throwError "division by zero"



-- | evaluator for statements
--  result is last assigned value
evalS :: Stmt -> Eval Int
evalS (Assign x e) = do
  v <- evalE e
  modify (Map.insert x v)
  return v

evalS (Seq c1 c2) = do
  evalS c1
  evalS c2
  


-- examples
ex1 = Mul (Const 23) (Add (Const 1) (Const 2))

ex2 = Div (Const 1) (Const 0)

ex3 = Seq (Assign "x" (Const 0))
          (Assign "y" (Div (Const 4) (Var "x")))

ex4 = (Seq
       (Seq
        (Assign "x" (Const 1))
        (Assign "x" (Div (Const 1) (Var "x")))
        )
       (Assign "x" (Const 2)))
      
           

