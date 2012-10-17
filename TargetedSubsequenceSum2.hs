{-|
Given a target integer t, and an array of integers, A[1..n], find
indexes i and j such that i <= j and A[i] + ... + A[j] = t, if such i and j
exist.

Here we present two implementations of an algorithm that requires
O(n * D(n)) time, where D(n) is the time to perform a lookup/insert on
a dictionary of n keys.

The algorithms iterates from index 1 through n, maintaining a sum of values
encountered and a dictionary mapping the sum at each index to its location.
At each index, it looks up in the dictionary the difference between the
current sum and the target value. If there is an entry, then we have a
solution, i.e. the range from the dictionary value + 1 to the current index.

Note: The results are indices that start at 1, while Haskell indexes lists
starting at 0.

The first solution uses IntMap and so D(n) = O(logn) and the algorithm takes
O(nlogn). The second solution uses HashTable and so D(n) = O(1) and the
algorithm takes O(n).  While the first solution incurs an O(logn) overhead,
it can take advantage of an early exit from the right fold once a solution
is fold.  The second solution uses a monadic fold which evaluates the entire
list for both a right and left fold and so does not early exit once
a solution is found.

It is possible to write both solutions 
-}

import Control.Applicative
import Control.Arrow
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import qualified Data.HashTable as Hash
import qualified Data.IntMap as IntMap
import Data.List
import Data.Maybe
import System.IO.Unsafe
import System.Random

{-|
We scanl summing the values and then zip this with the corresponding index
and build a map from sum to index. We then fold over the (sum, index).
The solution is either any previous solution, or we lookup the sum minus
the target in the dictionary.  If an entry exists, the solution
is the range+1 to the current index, or we continue with the fold.

The outer fold takes advantage of the fact that a right fold does not
evaluate the entire list if the evaluating function does not evaluate the
accumulator.
-}
solveTree :: Int -> [Int] -> Maybe (Int, Int)
solveTree t xs = foldr helper Nothing sums
    where dict = foldr (uncurry IntMap.insert) IntMap.empty sums
          sums = flip zip [0..] . scanl (+) 0 $ xs
          helper (s, j) acc = let res = do
                                    i <- IntMap.lookup (s-t) dict
                                    guard $ i < j
                                    return (i+1, j)
                              in  res <|> acc

{-|
The implementation is similar to above, only we use a HashTable instead of
an IntMap.  We use (MaybeT IO Int) to bind index i when computing the
solution range.
-}
solveHash t xs = unsafePerformIO $ do
  let sums = flip zip [0..] . scanl (+) 0 $ xs
  dict <- Hash.new (==) Hash.hashInt
  forM_ (reverse sums) $ uncurry (Hash.insert dict)
  let helper acc (s, j) = do
          res <- runMaybeT $ do
                   i <- MaybeT $ Hash.lookup dict (s-t)
                   guard $ i < j
                   return (i+1, j)
          return $! acc <|> res
  foldM helper Nothing sums

{-|
On the input set from [-100..100] verify that both implementations
give the same answer on each target [1..200].

Then we generate a list of 10 random integers in the range [-10..10]
inclusive, and a target integer, t, in the range [1..10], and then print
the result of both computations.
-}
main = do
  let a   = [-100..100]
  let res = all (flip solveTree a &&& flip solveHash a >>>
                 uncurry (==)) [1..200]
  putStrLn $ if res then "All tests passed." else "Some tests failed."

  putStrLn ""

  xs <- take 10 . randomRs ((-10)::Int, 10) <$> newStdGen
  t  <- randomRIO (1::Int, 10)
  putStrLn $ "xs: " ++ show xs
  putStrLn $ "t: " ++ show t
  let res1 = solveTree t xs
  let res2 = solveHash t xs
  print res1
  print res2
  putStrLn $ if res1 == res2 then "PASSED" else "FAILED!"

