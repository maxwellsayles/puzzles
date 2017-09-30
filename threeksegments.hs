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

segmentSums :: Num a => Int -> [a] -> [a]
segmentSums k xs = scanl (+) (sum $ take k xs) $ zipWith (-) (drop k xs) xs

maxSegment :: (Num a, Ord a) => Int -> [a] -> [a]
maxSegment k xs = scanl1 max $ segmentSums k xs

solve :: (Num a, Ord a) => Int -> [a] -> a
solve k xs =
  let ls = maxSegment k xs
      rs = drop (2 * k) $ reverse $ maxSegment k $ reverse xs
      ss = drop k $ segmentSums k xs
  in maximum $ zipWith (+) ls $ zipWith (+) ss rs

-- This is actually pretty poor. Probably O(k * n^3).
solvePoorly :: (Num a, Ord a) => Int -> [a] -> a
solvePoorly k xs =
  maximum [f a + f b + f c
          | a <- [0..n-3*k]
          , b <- [a+k..n-2*k]
          , c <- [b+k..n-k]
          ]
  where f i = sum $ take k $ drop i xs
        n = length xs

data TestInput = TestInput Int [Int] deriving Show

instance Arbitrary TestInput where
  arbitrary = sized $ \k -> do
    n <- choose (k * 3, k * 6)
    xs <- replicateM n arbitrary
    return $ TestInput k xs

testIt :: TestInput -> Bool
testIt (TestInput k xs) = solve k xs == solvePoorly k xs

main :: IO ()
main = quickCheck testIt
