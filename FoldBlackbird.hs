{- |
Continuations let you turn left associative operations into right
associative operations.
-}

import Data.List
import System.CPUTime

rightAssoc :: (a -> b -> c) -> (c -> d) -> a -> b -> d
rightAssoc f = \k x y -> k $ f x y

-- Or by lambdabot, we can write `rightAssoc` pointfree as...                         
-- rightAssoc = flip ((.) . (.))

-- Using Data.Aviary.Birds this is...
-- rightAssoc = flip blackbird

timeit x = do
  start <- getCPUTime
  return $! x
  end <- getCPUTime
  print $ end - start

main = do
  let n = 10000
  let xs = map return [1..n]
  timeit $ length $ foldl' (++) [] xs
  timeit $ length $ foldl' (rightAssoc (++)) id xs []
