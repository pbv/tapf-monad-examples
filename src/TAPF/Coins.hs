{-
  Case study for programming with error and list monads
  Coin change problem; greedy and exaustive solutions
  Pedro Vasconcelos, 2016
-}
module TAPF.Coins where

-- Greedy coin change algorithm
-- always chooses the largest possible coin first
greedy :: [Int] -> Int -> Maybe [Int]
greedy coins amount
  | amount == 0 = return []
  | null coins' = fail "no solution"
  | otherwise = do 
    cs <- greedy coins' (amount-c) 
    return (c:cs) 
  where coins' = filter (<=amount) coins
                -- all useable coins (less than-or-equal amount)                 
        c = maximum coins' -- greedy choice
                  
-- Exaustive algorithm: enumerate all solutions
-- repeats identical solutions (up-to ordering)
-- e.g.
-- exaustive [2,5] 9 = [[2,2,5],[2,5,2],[5,2,2]]
--
exaustive :: [Int] -> Int -> [[Int]]
exaustive coins amount
  | amount == 0 = return []
  | null coins' = fail "no solution"
  | otherwise = do
    c <- coins'
    cs <- exaustive coins' (amount-c)
    return (c:cs)
  where coins' = filter (<=amount) coins  -- usable coins


-- sample list of Euro coins (in cents)
euro_coins :: [Int]
euro_coins = [1,2,5,10,20,50,100,200]

