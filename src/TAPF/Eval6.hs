--
-- Evaluator, variation 6
-- more monad transformers
--

module Eval6 where

import           Data.Map (Map)
import qualified Data.Map as Map

import           Control.Monad.State
import           Control.Monad.Writer
import           Control.Monad.Except

type Name = String

data Expr = Const Int
          | Var Name
          | Add Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr
          | Div Expr Expr
           deriving Show

{- linguagem While  -}

data Stmt = Assign Name Expr
          | Block [Stmt]
          | While Expr Stmt
          | Print Expr
          deriving Show

-- efeitos
-- 1) estado (variáveis)
-- 2) exceções (divisões por zero)
-- 3) I/O ou pelo menos output
-- 4) "tick counting"

---
type Store = Map Name Int


-- the evaluation monad
-- combing state and exceptions, writer
-- stacking monads this way we preserve state after an exception
{-   Except --> StateT --> WriterT -> IO
-}
type Eval a = ExceptT String (StateT Store (WriterT (Sum Int) IO)) a

runEval :: Eval a -> Store -> IO ((Either String a, Store), Sum Int)
runEval m s0 = runWriterT (runStateT (runExceptT m) s0)

-- contar um passo de avaliação
tick :: Eval ()
tick = tell 1


-- | evaluator for expressions
evalE :: Expr -> Eval Int
evalE (Const n) = return n

evalE (Var x) = do
  store <- get
  case Map.lookup x store of
    Just v -> return v
    Nothing -> throwError ("unbound variable: " ++ show x)

evalE (Add e1 e2) = do
  v1 <- evalE e1
  v2 <- evalE e2
  return (v1+v2)

evalE (Sub e1 e2) = do
  v1 <- evalE e1
  v2 <- evalE e2
  return (v1-v2)

evalE (Mul e1 e2) = do
  v1 <- evalE e1
  v2 <- evalE e2
  return (v1*v2)

evalE (Div e1 e2)  = do
  v1 <- evalE e1
  v2 <- evalE e2
  if v2 /= 0 then
    return (v1 `div` v2)
    else
    throwError "division by zero"

-- | evaluator for statements
--  result is last assigned value
evalS :: Stmt -> Eval ()
evalS (Block stms) = sequence_ (map evalS stms)
                    -- mapM_ evalS stms
evalS (Assign var expr) = do
  tick
  val <- evalE expr
  -- modify :: (Store -> Store) -> Eval ()
  modify (Map.insert var val)

evalS (Print expr) = do
  tick
  val <- evalE expr
  lift $ lift $ lift $ print val

evalS (While expr body) = do
  tick
  val <- evalE expr
  loop val
  where
    loop 0 = return ()
    loop _ = do
      tick
      evalS body
      val <- evalE expr
      loop val



-- examples
ex1 = Mul (Const 23) (Add (Const 1) (Const 2))

ex1b = Mul (Var "x") (Add (Const 1) (Const 2))

ex2 = Div (Const 1) (Const 0)


ex3 = Block [ Assign "n" (Const 10)
            , Assign "r" (Const 1)
            , While (Var "n")
              (Block [ Print (Var "n")
                     , Assign "r" (Var "r" `Mul` Var "n")
                     , Assign "n" (Var "n" `Sub` Const 1)
                     ])
            , Print (Var "r")
            ]

ex4 = Block [ Assign "a" (Const 1)
            , Print (Var "a")
            , Assign "a" (Var "a" `Add` (Const 1))
            , Print (Var "a")
            , Assign "b" (Var "a" `Add` Const 1)
            ]
