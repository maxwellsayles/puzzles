{--
Given an array of integers, find 3 non-overlapping segments each of k elements
where the sum of the elements covered by the segments is maximal.

We present 3 solutions. Each solution makes use of a common function to
precompute segment sums ta each starting position.

`solvePoorly` implements the straightforward O(n^3) approach of placing each
segment in each position.

`solve` improves on this slightly by computing for each position, the maximum
segment sum to the left and to the right of this position. Then it needs only
to place the middle segment at each position, and it can efficiently look up
the maximum segment sum of the segment to the left and to the right. This
requires O(n) time.

`solveDP` iteratively builds a solution one segment at a time for each position.
First it computes the maximum segment sum for a single segment. Then for each
position, it computes the maximum that can be earned by placing a second segment
in each position and using the previous solution, it can efficiently compute
the maximum for a solution to the left on one fewer segment. This is then
repeated for the third segment. This solution is also O(n), but has the
additional property that it can be used to generate solutions for arbitrary `c`
segments in O(nc) time.
-}

import Control.Monad
import Data.Foldable
import Test.QuickCheck

import qualified Data.Vector as V

segmentSums :: Num a => Int -> [a] -> [a]
segmentSums k xs = scanl (+) (sum $ take k xs) $ zipWith (-) (drop k xs) xs

solve :: (Num a, Ord a) => Int -> [a] -> a
solve k xs =
  let ss = segmentSums k xs
      ls = scanl1 max ss
      rs = scanr1 max $ drop (2 * k) ss
      ms = drop k ss
  in maximum $ zipWith (+) ls $ zipWith (+) ms rs

solveDP :: (Num a, Ord a) => Int -> [a] -> a
solveDP k xs = maximum $ loop (segmentSums k xs) (repeat 0) 3
  where loop _ ms 0 = ms
        loop ss ms c =
          let ss' = drop k ss
              ms' = scanl1 max $ zipWith (+) ss ms
          in  loop ss' ms' (c - 1)

{-
This is actually pretty poor. It takes advantage of precomputing the segment
sums, but it's still O(n^3).
-}
solvePoorly :: (Num a, Ord a) => Int -> [a] -> a
solvePoorly k xs =
  maximum [yvec V.! a + yvec V.! b + yvec V.! c
          | a <- [0..n-2*k]
          , b <- [a+k..n-k]
          , c <- [b+k..n]
          ]
  where n = length ys - 1
        ys = segmentSums k xs
        yvec = V.fromList ys
        
data TestInput = TestInput Int [Int] deriving Show

instance Arbitrary TestInput where
  arbitrary = sized $ \k -> do
    n <- choose (k * 3, k * 6)
    xs <- replicateM n arbitrary
    return $ TestInput k xs

testIt :: TestInput -> Bool
testIt (TestInput k xs) =
  solve k xs == solvePoorly k xs &&
  solve k xs == solveDP k xs

main :: IO ()
main = quickCheckWith (stdArgs { maxSuccess = 1000 }) testIt
