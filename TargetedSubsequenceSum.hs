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
current sum and the target value.  If there is an entry, then we have a
solution, i.e. the range from the dictionary value + 1 to the current index.

Note: The dictionary is initialized with a sum of 0 and an index of -1,
since when a dictionary lookup succeeds, the value refers to the inclusive
index at the end of the scan that sums to the key. In this case, if the
range [0..j] sums to t, we lookup s-t=0 and get back -1 and the solution is
on (-1 + 1, j).

The first solution uses IntMap and so D(n) = O(logn) and the algorithm takes
O(nlogn).  The second solution uses HashTable and so D(n) = O(1) and the
algorithm takes O(n).
-}

import Control.Applicative
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import qualified Data.HashTable as Hash
import qualified Data.IntMap as IntMap
import Data.Maybe
import System.IO.Unsafe
import System.Random

{-|
/loop/ does a left fold over the list, while maintaining the index j,
the current sum, and a map of sums to indexes i.
/a/ uses the Maybe monad and looks up the current sum minus the target
and returns the range (i+1, j).  /b/ recurses on the remainder of the list.
The solution is /a/ if successful, otherwise /b/.
-}
solveTree :: Int -> [Int] -> Maybe (Int, Int)
solveTree t xs = loop xs 0 0 baseMap
    where baseMap = IntMap.singleton 0 (-1)
          loop []     _ _ _ = Nothing
          loop (x:xs) j s m =
              let s' = s + x
                  a  = do i <- (s' - t) `IntMap.lookup` m
                          return (i+1, j)
                  b  = loop xs (j+1) s' $ IntMap.insert s' j m
              in  a <|> b

{-|
The solution is similar to above, only we use HashTable instead of IntMap.
/a/ uses (MaybeT IO Int) to bind the index i and compute the result (i+1,j).
/b/ modifies the hash to contain the new sum and then recurses using /loop/.
As above, the solution is /a/ if successful, otherwise /b/.  Since Haskell
is lazily evaluated, /b/ is not evaluated unless /a/ fails.
-}
solveHash :: Int -> [Int] -> Maybe (Int, Int)
solveHash t xs = unsafePerformIO $ do
  m <- Hash.new (==) Hash.hashInt
  Hash.insert m 0 (-1)
  let loop []     _ _ = return Nothing
      loop (x:xs) j s = do
        let s' = s + x
        a <- runMaybeT $ do i <- MaybeT $ Hash.lookup m (s' - t)
                            return (i+1, j)
        b <- do Hash.insert m s' j
                loop xs (j+1) s'
        return $! a <|> b 
  loop xs 0 0

{-|
Generate a list of 10 random integers in the range [-10..10] inclusive,
and a target integer, t, in the range [1..10], and then print
the result of both computations.
-}
main = do
  xs <- take 10 . randomRs ((-10)::Int, 10) <$> newStdGen
  t  <- randomRIO (1::Int, 10)
  putStrLn $ "xs: " ++ show xs
  putStrLn $ "t: " ++ show t
  print $ solveTree t xs
  print $ solveHash t xs