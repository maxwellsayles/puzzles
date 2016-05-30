{- | Return the longest increasing subsequence for a given input array.

We implement two algorithms, one in O(n^2) time and the other in O(nlogn) time.
Both are O(n) space.

The first is DP and extends a solution by searching all previous solutions
ending with an integer <= the current integer and takes one of maximum length.

  opt ! i = 1 + maximum [opt ! j | j <- [1..i-1] and A ! j < A ! i]

This runs in O(n^2) time and O(n) memory.

The second one maintains a map where the values are the longest subsequence
ending with the key.  We iterate over the input updating the map.  For input
element x, we find the longest sequence ending with y < x and insert a new
sequence with x appended.  But before inserting this new sequence, we remove the
sequence with the same length as the new sequence by search for the subsequence
ending with an element z >= x.

  opt ! x = M.insert x (x:ys) $ M.delete z opt
  where ys = snd $ M.lookupLT x opt
        z = fst $ M.lookupGE x opt

-}

import Data.List
import qualified Data.Map.Lazy as M
import Data.Ord
import Test.QuickCheck

solveDp :: Ord a => [a] -> [a]
solveDp [] = []
solveDp xs = reverse $ snd $ maximumBy (comparing fst) $ foldl' step [] xs
  where step acc x =
          let ps = filter ((< x) . head . snd) acc
              (n, ys) | null ps = (0, [])
                      | otherwise = maximumBy (comparing fst) ps
          in (n + 1, x : ys) : acc

solveMap :: Ord a => [a] -> [a]
solveMap [] = []
solveMap xs = reverse $ snd $ head $ M.toDescList $ foldl' step M.empty xs
  where step m x =
          let ys = maybe [] snd $ M.lookupLT x m
              m' = maybe m (flip M.delete m . fst) $ M.lookupGE x m
          in M.insert x (x:ys) m'

verify :: Ord a => [a] -> Bool
verify xs = let ys = solveDp xs
                zs = solveMap xs
            in length ys == length zs && sort ys == ys && sort zs == zs

main :: IO ()
main = let args = stdArgs { maxSuccess = 1000, maxSize = 1000 }
       in quickCheckWith args (verify :: [Int] -> Bool)
