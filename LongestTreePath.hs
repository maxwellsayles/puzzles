{-
Given a binary tree, find the longest path between two nodes.

We solve this by computing the height of a tree.  Given the height
of the left subtree and the height of the right subtree, the longest
path for a subtree that includes the root of the subtree is the sum
of the height of the two subtrees.  The longest path for the tree is
the longest path over all subtrees.

Since the height of a tree can be computed in O(n), we can do determine
the longest path in O(n).
-}

{-# LANGUAGE FlexibleInstances #-}

import Control.Arrow
import Control.Monad.State
import System.Random
import Test.QuickCheck

data Tree a = Empty | Node a (Tree a) (Tree a)

instance Arbitrary (Tree Int) where
  arbitrary = do
    low <- arbitrary
    high <- sized (return . (low +))
    randomTree low high

{-
Generate a random binary search tree that includes each of the
elements in the range [low, high] inclusive.
-}
randomTree :: (Ord a, Random a, Num a) => a -> a -> Gen (Tree a)
randomTree low high
    | low > high  = return Empty
    | low == high = return $! Node low Empty Empty
    | otherwise   = do
        mid <- choose (low, high)
        left <- randomTree low (mid-1)
        right <- randomTree (mid+1) high
        return $! Node mid left right

{- Returns (height, longestPathSoFar) -}
longestPath :: (Num b, Ord b) => Tree a -> (b, b)
longestPath Empty = (0, 0)
longestPath (Node _ l r) =
    let (lh, lx) = longestPath l
        (rh, rx) = longestPath r
    in  (1 + max lh rh, maximum [lx, rx, lh + rh + 2])

main :: IO ()
main = do
  return ()
