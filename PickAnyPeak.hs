{-- A peak is an index `i` such that `arr ! (i - 1) < arr ! i < arr ! (i + 1)`.
This uses a O(logn) approach to find any peak. Out of bounds is considered to
be -\inf.
--}

import Data.Array.IArray
import Data.Array.Base (UArray)
import Data.List
import Test.QuickCheck

pickAnyPeak :: (Integral ix, Ix ix, Ord e, Enum e, IArray arr e)
            => arr ix e -> ix
pickAnyPeak arr = fnc x y
  where (x, y) = bounds arr
        fnc i j
          | b < c = fnc (m + 1) j
          | b < a = fnc i (m - 1)
          | otherwise = m
          where m = i + (j - i) `div` 2
                a = if m == x then pred b else arr ! (m - 1)
                b = arr ! m
                c = if m == y then pred b else arr ! (m + 1)

isPeak :: (Enum e, Ord e, Num ix, Ix ix, IArray arr e) => arr ix e -> ix -> Bool
isPeak arr i = a < b && b > c
  where (x, y) = bounds arr
        a = if i == x then pred b else arr ! (i - 1)
        b = arr ! i
        c = if i == y then pred b else arr ! (i + 1)

testPeak :: [Int] -> Bool
testPeak [] = True
testPeak list = isPeak arr peak
  where list' = nub list
        n = length list'
        arr = listArray (1, n) list' :: UArray Int Int
        peak = pickAnyPeak arr

main :: IO ()
main = quickCheckWith stdArgs { maxSuccess = 10000 } testPeak
