{-- A peak is an index `i` such that `arr ! (i - 1) < arr ! i < arr ! (i + 1)`.
This uses a O(logn) approach to find any peak. Out of bounds is considered to
be \inf.
--}

import Data.Array.IArray
import Data.Array.Base (UArray)
import Data.List
import Test.QuickCheck

pickAnyPeak :: (Integral ix, Ord e, Ix ix, IArray arr e) => arr ix e -> Maybe ix
pickAnyPeak arr = fnc x y
  where (x, y) = bounds arr
        fnc i j
          | m == x = Nothing
          | m == y = Nothing
          | arr ! m < arr ! (m + 1) = fnc (m + 1) j
          | arr ! m < arr ! (m - 1) = fnc i (m - 1)
          | otherwise = Just m
          where m = i + (j - i) `div` 2

isPeak :: (Num ix, Ord e, Ix ix, IArray arr e) => arr ix e -> ix -> Bool
isPeak arr i
  | i - 1 < x = False
  | i + 1 > y = False
  | otherwise = arr ! (i - 1) < arr ! i && arr ! i > arr ! (i + 1)
  where (x, y) = bounds arr

testPeak :: [Int] -> Bool
testPeak list =
  let list' = nub list in
  let arr = listArray (1, length list') list' :: UArray Int Int in
  let peak = pickAnyPeak arr in
  case peak of
    Just p -> isPeak arr p
    Nothing -> True

main :: IO ()
main = quickCheckWith stdArgs { maxSuccess = 10000 } testPeak
