{-|
Given an input array of arbitrary integers, compute the largest subset where no
two elements are adjacent in the array.

The `largestNonAdjacentSum` method should run in O(n) time and O(1) space. The
principal is that the largest possible sum for an array of `i` elements either
includes the `i`th element or it does not.  If it does, then the maximal value
is the largest sum on `i-2` elements plus the `i`th element or the largest sum
on `i-1` elements.

The `solvePoorly` method computes all possible combinations of either including
or exluding non-adjacent elements and sums them.
|-}
import Control.Monad (replicateM)
import Data.List (nub)
import Test.QuickCheck

largestNonAdjacentSum :: (Num a, Ord a) => [a] -> a
largestNonAdjacentSum [] =0
largestNonAdjacentSum xs = helper 0 0 xs
  where
    helper n2 n1 [x] = max n1 (x + n2)
    helper n2 n1 (x:xs) =
      let n0 = max n1 (x + n2)
      in helper n1 n0 xs

allNonAdjacentSubsets [] = [[]]
allNonAdjacentSubsets [x] = [[], [x]]
allNonAdjacentSubsets (x:xs) =
  let n2 = allNonAdjacentSubsets $ tail xs
      n1 = allNonAdjacentSubsets xs
      n0 = map (x:) n2
  in n0 ++ n1 ++ n2

solvePoorly :: (Num a, Ord a) => [a] -> a
solvePoorly = maximum . map sum . nub . allNonAdjacentSubsets

data TestInput = TestInput [Int] deriving Show

instance Arbitrary TestInput where
  arbitrary = sized $ \k -> do
    xs <- replicateM k arbitrary
    return $ TestInput xs

testIt :: TestInput -> Bool
testIt (TestInput xs) =
  largestNonAdjacentSum xs == solvePoorly xs

main :: IO ()
main = quickCheckWith (stdArgs { maxSuccess = 1000, maxSize = 16 }) testIt
