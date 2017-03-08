{-
  Solve the cryto arithmetic puzzle:
      SEND
    + MORE
   -------
   = MONEY
  Find an assignment of distinct digits 0..9 to letters such that
  the addition above holds; uses the list monad for backtracking search 

   Pedro Vasconcelos <pbv@dcc.fc.up.pt>, 2015
-}

module TAPF.Puzzle where

import           Control.Monad
import           Control.Monad.State
import           Data.List ((\\), foldl')

-- naive solution with "deep" backtracking search
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
  


