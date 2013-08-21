{-
Compute a tree of the same shape where the value of each leaf
is the minimum value of the input tree.

Normally this is computed in two passes: first find the minimum
value, second regenerate the tree.

Here, helper returns the minimum value in the subtree, and
a subtree of the same shape where each leaf takes the global
minimum value.

This runs in linear time in a single pass.  Laziness is cool!
-}

data Tree = Leaf Int | Node Tree Tree deriving Show

-- Use two pases: first find the min, second replace each leaf
makeMinTree' t = replace t
    where mx = treeMin t
          treeMin (Leaf x)   = x
          treeMin (Node l r) = min (treeMin l) (treeMin r)
          replace (Leaf _)   = Leaf mx
          replace (Node l r) = Node (replace l) (replace r)

-- Generate a tree of the same shape where each leaf has the min
-- value.  'helper' returns the min value for the subtree and a
-- subtree of the same shape where each leaf has the min global
-- value that is lazily computed.
makeMinTree t = t'
    where (mx, t') = helper t
          helper (Leaf x) = (x, Leaf mx)
          helper (Node l r) = let (ml, l') = helper l
                                  (mr, r') = helper r
                              in  (min ml mr, Node l' r')

main = do
  let t = Node (Node (Node (Leaf 3) (Leaf 2)) (Node (Leaf 1) (Leaf 4))) (Leaf 5)
  print t
  print $ makeMinTree  t
  print $ makeMinTree' t


