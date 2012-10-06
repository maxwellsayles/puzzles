{-|
  Given a max-heap, i.e. a binary tree with the property
  that children nodes are <= to their parent nodes, and
  not necessarily balanced, we can serialize this heap
  using an in order traversal.

  The problem is to deserialize the list to the original
  heap.

  We present here a solution that runs in O(n) for both
  serialization and deserialization.
-}

import Control.Arrow
import System.Random

data Tree a = Empty | Node (Tree a) a (Tree a)
              deriving Show

-- Given a heap that has been serialized with an in order traversal,
-- this computes the original heap.
-- helper takes a bound, b, and consumes x < b from the list.
-- It returns the heap generated from the first part of the list,
-- and the remainder of the list.
-- The recursion is such that the heap returned by helper is a 
-- right child of the heap with x as its root and the accumulated
-- heap as its left child.
deserialize xs = fst $ helper (maximum xs) Empty xs
    where helper _ acc [] = (acc, [])
          helper b acc xss@(x:xs)
              | x > b = (acc, xss)
              | otherwise = uncurry (helper b) $
                            first (Node acc x) $
                            helper x Empty xs


-- The in order traversal of a binary tree.
-- The method below is the difference list implementation of the following:
--
-- serialize Empty = []
-- serialize (Node l v r) = serialize l ++ [v] ++ serialize r
serialize t = helper t []
    where helper Empty = id
          helper (Node l v r) = helper l . (v:) . helper r

-- Generate a list of random integers that represents a serialized heap.
-- Generate a heap from the list, and then serialize it again
-- and compare it with the original.
main = do
  let n = 100000
  xs <- return . take n . randomRs (0::Int, 99) =<< newStdGen
  let ys = serialize $ deserialize xs
  print $ xs == ys
