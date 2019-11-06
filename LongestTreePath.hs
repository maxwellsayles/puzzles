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

import Control.Arrow
import Control.Monad.State
import System.Random

data Tree a = Empty | Node a (Tree a) (Tree a)

{-
Generate a random binary search tree that includes each of the
elements in the range [low, high] inclusive.
-}
randomTree low high
    | low > high  = return Empty
    | low == high = return $! Node low Empty Empty
    | otherwise   = do mid <- state $ randomR (low, high)
                       left <- randomTree low (mid-1)
                       right <- randomTree (mid+1) high
                       return $! Node mid left right

{- Returns (height, longestPathSoFar) -}
longestPath Empty = (0, 0)
longestPath (Node _ l r) =
    let (lh, lx) = longestPath l
        (rh, rx) = longestPath r
    in  (1 + max lh rh, maximum [lx, rx, lh+rh+2])

main = print .
       snd .
       longestPath .
       evalState (randomTree (0::Int) 100000) =<<
       newStdGen

