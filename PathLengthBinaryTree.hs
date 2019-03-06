{- Find the path length between two nodes in a binary tree.

The solution finds the path from the root to node X, and the path from root to
node Y. Then removes the common path prefix, except for the last common element,
and then it's a simple concatonation.
-}

data Tree = Empty | Node Int Tree Tree

pathTo :: Int -> Tree -> [Tree]
pathTo x = reverse . helper []
  where
    helper acc Empty = []
    helper acc n@(Node id l r)
      | id == x = acc'
      | otherwise =
          case (helper acc' l, helper acc' r) of
            (xs, []) -> xs
            ([], ys) -> ys
      where acc' = n:acc

pathBetween :: Int -> Int -> Tree -> [Tree]
pathBetween x y root =
  let pathx = pathTo x root
      pathy = pathTo y root
      common = takeWhile (\(Node idx _ _, Node idy _ _) -> idx == idy) $ zip pathx pathy
      n = length common
  in reverse (drop (n - 1) pathx) ++ drop (n - 1) pathy
