{-
  Example programming with the state monad:
  Labelling tree nodes with unique integers;
  Pedro Vasconcelos, 2016
-}
module TAPF.Labelling where

import Control.Monad.State

-- | binary trees with annotations on branches
data Tree a = Leaf   -- ^ empty tree
            | Node a (Tree a) (Tree a)  -- ^ branch with 2 sub trees
            deriving (Eq, Show)


-- | label nodes in a tree with unique integers;
-- first version, with explicit state threading
-- top-level function
label :: Tree a -> Tree (a,Int)
label t = fst (labelAux t 0)

-- recursive "worker" function;
-- second argument is the input counter;
-- the result is a pair of tree and output counter
labelAux ::  Tree a -> Int -> (Tree (a,Int), Int)
labelAux Leaf c = (Leaf, c)
labelAux (Node x left right) c0 =
  let (left', c1) = labelAux left c0
      (right', c2)= labelAux right c1
      t = Node (x,c2) left' right'
      c3 = 1 + c2 
  in (t, c3)



-- | second version using the state monad 
label' :: Tree a -> Tree (a,Int)
label' t = fst (runState (labelAux' t) 0)

-- recursive "worker"; 
-- counter "threading" is hidden in the monad
labelAux' :: Tree a -> State Int (Tree (a,Int))
labelAux' Leaf = return Leaf
labelAux' (Node x left right) = do
  left' <- labelAux' left
  right'<- labelAux' right
  c <- get  -- get current counter
  put (c+1) -- and increment it
  return (Node (x,c) left' right')

-- a sample tree
example = (Node 'a'
           (Node 'b' Leaf Leaf)
            (Node 'c' Leaf Leaf))

