{--
Given an array of integers, find 3 non-overlapping segments each of k elements
where the sum of the elements covered by the segments is maximal.

The strategy is this. First, from left to right, walk one segment of k elements.
At each index, store the largest sum that can be made with a segment ending at
that index. Do the same from right to left. Then for each index, place the
middle segment and in constant time, look up the largest sum made by a segment
covering it the section to the left and to the right. Since each stage takes
O(n) time and requires O(n) memory, the solution is O(n) time and memory.
-}

import Control.Monad
import Data.Foldable
import Test.QuickCheck

import qualified Data.Vector as V

segmentSums :: Num a => Int -> [a] -> [a]
segmentSums k xs = scanl (+) (sum $ take k xs) $ zipWith (-) (drop k xs) xs

solve :: (Num a, Ord a) => Int -> [a] -> a
solve k xs =
  let ls = scanl1 max $ segmentSums k xs
      rs = scanr1 max $ drop (2 * k) $ segmentSums k xs
      ss = drop k $ segmentSums k xs
  in maximum $ zipWith (+) ls $ zipWith (+) ss rs

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
testIt (TestInput k xs) = solve k xs == solvePoorly k xs

main :: IO ()
main = quickCheckWith (stdArgs { maxSuccess = 1000 }) testIt
