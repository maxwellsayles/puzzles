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
is found.  The second solution uses a monadic fold which evaluates the entire
list for both a right and left fold and so does not early exit once
a solution is found.
-}

import Control.Applicative
import Control.Monad.ST
import qualified Data.HashTable.ST.Basic as Hash
import qualified Data.IntMap as IntMap
import System.Random

{-|
We scanl summing the values and then zip this with the corresponding index.
Then we scanl over this to build a map from sum to index. We then fold over
the (sum, index).  We lookup the sum minus the target in the dictionary.
If an entry exists, the solution is the range+1 to the current index, or we
continue with the fold.

The fold takes advantage of the fact that a right fold does not evaluate the
entire list if the evaluating function does not evaluate the accumulator.  Also
scanl lazily builds the results from left to right, and so can be early
terminated once a solution is found.
-}
solveTree :: Int -> [Int] -> Maybe (Int, Int)
solveTree t xs = foldr helper Nothing (zip sums dicts)
  where
    dicts = scanl (\m (v, k) -> IntMap.insert k v m) IntMap.empty sums
    sums = zip [0..] $ scanl (+) 0 xs
    helper ((j, s), m) acc =
      let res = do
            i <- IntMap.lookup (s - t) m
            return (i + 1, j)
      in  res <|> acc

{-|
This implementation is conceptually similar to above, only we use a HashTable
instead of an IntMap, and the loop is explicit rather than a fold.
-}
solveHash :: Int -> [Int] -> Maybe (Int, Int)
solveHash t xs = runST $ do
  let sums = zip [0..] $ scanl (+) 0 xs
  dict <- Hash.new
  let loop [] = return Nothing
      loop ((j, s) : xs') = do
        m <- Hash.lookup dict (s - t)
        case m of
          Nothing -> do
            Hash.insert dict s j
            loop xs'
          Just i -> return $ Just (i + 1, j)
  loop sums

{-|
Computes the sum of all values for the given indices.
NOTE: Input indices start at 1, so we have to subtract 1 for list indices.
-}
verify :: Int -> [Int] -> (Int, Int) -> Bool
verify t xs (i, j) = (==t) $ sum $ take (j - i + 1) $ drop (i - 1) xs

{-|
On the input set from [-100..100] verify that both implementations
give the same answer on each target [1..200].

Then we generate a list of 10 random integers in the range [-10..10]
inclusive, and a target integer, t, in the range [1..10], and then print
the result of both computations.
-}
main :: IO ()
main = do
  let a   = [-100..100]
  let res = all (\t -> (verify t a <$> solveTree t a) ==
                       (verify t a <$> solveHash t a))
                [1..200]
  putStrLn $ if res then "All tests passed." else "Some tests failed."
  putStrLn ""

  putStrLn "Using a random example of 10 inputs."
  xs <- take 10 . randomRs ((-10)::Int, 10) <$> newStdGen
  t  <- randomRIO (1::Int, 10)
  putStrLn $ "xs: " ++ show xs
  putStrLn $ "t: " ++ show t
  let res1 = solveTree t xs
  let res2 = solveHash t xs
  putStrLn $ "Tree: " ++ show res1
  putStrLn $ "Hash: " ++ show res2
  let res1' = verify t xs <$> res1
  let res2' = verify t xs <$> res2
  putStrLn $ if res1' == res2' then "PASSED" else "FAILED!"



