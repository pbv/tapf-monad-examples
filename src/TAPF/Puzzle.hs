{-
  Solve arithmetic puzzles.

  "Find an assignment of distinct digits 0..9 to letters such that
   the following addition holds:
      SEND
    + MORE
   -------
   = MONEY

  Two solutions using the list monad for backtracking search.

  Pedro Vasconcelos <pbv@dcc.fc.up.pt>, 2015-2017
-}

module TAPF.Puzzle where

import           Control.Monad
import           Control.Monad.State
import           Data.List ((\\), foldl')
import           Data.Map (Map)
import qualified Data.Map as Map

--
-- | Solution 1
-- specific solution for SEND + MORE = MONEY
-- naive (deep) backtracking search
-- 
solveNaive :: [(Int,Int,Int)]
solveNaive = do
  s <- [1..9]
  e <- [0..9] \\ [s]
  n <- [0..9] \\ [s, e]
  d <- [0..9] \\ [s, e, n]
  m <- [1..9] \\ [s, e, n, d]
  o <- [0..9] \\ [s, e, n, d, m]
  r <- [0..9] \\ [s, e, n, d, m, o]
  y <- [0..9] \\ [s, e, n, d, m, o, r]
  let send = toNumber [s,e,n,d]
  let more = toNumber [m,o,r,e]
  let money = toNumber [m,o,n,e,y]
  guard (send + more == money)
  return (send, more, money)
    

-- convert a list of digits to a number
toNumber :: [Int] -> Int
toNumber = foldl' (\x y -> x*10+y) 0

---
--- | Solution 2
--- using shallow backtracking
---
-- | letter assignments
type Letter = Char
type Digit = Int
type Assign = Map Letter Digit


-- | monad transformer for backtracking search
-- combinatio of state (letter assignments) and
-- non-determinism (list of alternatives)
--
type Search a = StateT Assign [] a

-- | labelling the available choices for a letter
labelling :: Letter -> Search Digit
labelling x = do
  s <- get
  case Map.lookup x s of
    Just v -> return v  -- single choice
    Nothing -> let vs = [0..9] \\ Map.elems s
                        -- remaing alternatives
               in msum [do modify (Map.insert x v); return v
                       | v <- vs]

-- | worker function to do ripple-carry addition
solveAux :: [Letter] -> [Letter] -> [Letter] -> Digit -> Search ()
-- base cases ---------
solveAux [] [] []  carry =
  guard (carry == 0)  -- no more digits: final carry must be zero
solveAux [] [] [z] carry = do
  vz <- labelling z
  guard (vz == carry) -- last digit must equal carry
-- recursive case --------
solveAux (x:xs) (y:ys) (z:zs) carry =
  do vx <- labelling x
     vy <- labelling y
     vz <- labelling z
     guard ((vx + vy + carry)`mod`10 == vz)  -- column addition
     let carry' = (vx + vy + carry)`div`10   -- next carry
     solveAux xs ys zs carry'

-- | top-level solving wrapper
solveTop :: [Letter] -> [Letter] -> [Letter] -> Search ()
solveTop xs ys zs = do
  -- reverse digits for ripple adder
  solveAux (reverse xs) (reverse ys) (reverse zs) 0
  -- ensure most significant digits are not zero
  vx <- labelling (head xs)
  vy <- labelling (head ys)
  vz <- labelling (head zs)
  guard (vx/=0 && vy/=0 && vz/=0)

-- | fast generic solver; try e.g.
-- > solveFast "SEND" "MORE" "MONEY"
-- > solveFast "GREAT" "SWERC" "PORTO"
--
solveFast :: [Letter] -> [Letter] -> [Letter] -> [Assign]
solveFast xs ys zs = map snd $ runStateT (solveTop xs ys zs) Map.empty



