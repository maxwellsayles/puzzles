{- |
Continuations let you turn left associative operations into right
associative operations.

This is particularly useful for (++) when using foldl or any operator whose
runtime depends on the size of the left input, as it effectively converts the
foldl into a foldr.

Special attention should be given for (-) or any operator that does not produce
the same result with foldl and foldr.
-}

import Data.List
import System.CPUTime

rightAssoc :: (a -> b -> c) -> (c -> d) -> a -> b -> d
rightAssoc = \f k x y -> k $ f x y

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
