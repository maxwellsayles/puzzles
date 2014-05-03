{-|
You are given a plank of length w and a list xs where you must cut the plank.
The cost of each cut is the cost of the length of the segment at the time
of the cut.  For example, support

  w = 10
  xs = [2, 5, 7]

If you were to first cut at 2, then at 5, and then at 7, the cost would be
  10 + 8 + 5 = 23.
But if you were to first cut at 5, then at 2, and dthen at 7, the cost be
  10 + 5 + 5 = 20.

Find the optimal cost to make all the cuts.

The exhaustive approach tries each of the cuts, and recurses on the left
and right segments created.

The dynamic programming approach is exactly the same, but memoizes the
results.  The DP solution is also computed from the bottom up: we first
compute all segments containing 1 cut, then 2 cuts, up to all n cuts.  The
DP solution should take O(n^2) time and memory.
-}

import Control.Applicative
import Control.Monad
import Data.Array.IArray
import Data.List
import Test.QuickCheck

solveExhaustive :: [Int] -> Int -> Int
solveExhaustive xs w = loop 0 n'
  where
    n = length xs
    n' = n + 1

    arr :: Array Int Int
    arr = listArray (0, n') $ 0 : xs ++ [w]

    loop :: Int -> Int -> Int
    loop i j
      | i + 1 >= j = 0
      | otherwise = foldl' min (head costs) (tail costs)
      where
        costs = [intervalCost k | k <- [i + 1 .. j - 1]]
        intervalCost k =
          let w' = (arr ! j) - (arr ! i)
          in  w' + loop i k + loop k j

solveDP :: [Int] -> Int -> Int
solveDP xs w = opt ! (0, n')
  where
    n = length xs
    n' = n + 1

    arr :: Array Int Int
    arr = listArray (0, n') $ 0 : xs ++ [w]

    opt :: Array (Int, Int) Int
    opt = array ((0, 0), (n', n')) $
          [((i, j), eval i j) |
           len <- [0 .. n'],
           i <- [0 .. n' - len],
           let j = i + len]

    eval :: Int -> Int -> Int
    eval i j
      | i + 1 >= j = 0
      | otherwise = foldl' min (head costs) (tail costs)
      where
        costs = [intervalCost k | k <- [i + 1 .. j - 1]]
        intervalCost k = w + opt ! (i, k) + opt ! (k, j)
        w = arr ! j - arr ! i

data TestInput = TestInput [Int] Int deriving Show

instance Arbitrary TestInput where
  arbitrary = sized $ \n -> do
    w <- abs <$> arbitrary
    xs <- (nub . sort) <$> replicateM n (choose (1, w))
    return $! TestInput xs (w + 1)

verify (TestInput xs w) = solveExhaustive xs w == solveDP xs w

main = do
  let args = stdArgs { maxSuccess = 100, maxSize = 16 }
  verboseCheckWith args verify
