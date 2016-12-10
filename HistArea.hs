{-|
Given a histogram with positive integral values, find a rectangle of largest
area contained within the histogram.

We provide three approaches.  A straighforward approach of O(n^2), a
divide-and-conquer approach of O(nlogn), and an approach using precomputation
that takes O(n).

A straightforward solution is for each index i, and each index j >= i, consider
the largest rectangle from [i,j] contained in the histogram.  We can do this in
O(n^2) time by iterating i and j and updating the rectangle height with each new
j.

The divide-and-conquer approach divides the list in two halves and recurses on
each half.  Assume we know the largest rectangle on the left and right.  We then
compute the largest largest rectangle that straddles the two halves.  We do this
by iterating from the middle outwards.  At each iteration, we either append the
next cell to the left or right, whichever is taller, and then adjust the height
of the rectangle appropriately.  Since there are O(logn) levels and each level
requires O(n) operations, this gives O(nlogn) runtime.

The approach using precomputation requires us to first compute the nearest index
to the left and right of each cell with a value < the given cell.  To compute
the left variant, we start at the leftmost cell.  There is no possible index to
the left.  This is the base case.  Then we iterate to the right.  For each cell,
check the computed value of the cell immediately to the left.  It either has no
lower value, in which case, the current cell has no lower value, or we get the
nearest index to the left with a lower value.  We jump to that index and check
its value.  If it is less than the initial cell, we stop.  Otherwise we repeat.
Since each cell is visited at most once, this is O(n) time.

After precomputing, for each cell, we can quickly look up the index to the left
and right with a value < the given cell.  We use this to compute the rectangle
of maximum area of the hegiht of that cell.   We can do this in O(n) time as
well.
-}

import Control.Monad
import Data.Array.IArray
import Data.List
import Data.Maybe
import Data.Ord
import Test.HUnit
import Test.QuickCheck

-- | Compute the nearest index to the left with a value < the given index.
nearestLT :: [Int] -> [Maybe Int]
nearestLT xs = elems opt
  where
    n = length xs

    xs' :: Array Int Int
    xs' = listArray (0, n - 1) xs

    opt :: Array Int (Maybe Int)
    opt = listArray (0, n - 1) $ Nothing : zipWith helper (tail xs) [0..]
      
    helper x i
      | xs' ! i < x = return i
      | otherwise = helper x =<< opt ! i

-- | Compute the nearest index to the right with a value < the given index.
nearestGT :: [Int] -> [Maybe Int]
nearestGT xs = map (fmap (n - 1 -)) $
               reverse $
               nearestLT $
               reverse xs
  where n = length xs

-- | `start` and `end` are inclusive
data Solution = Solution { start :: Int
                         , end :: Int
                         , height :: Int
                         } deriving (Eq, Show)

instance Ord Solution where
  s1@(Solution i1 j1 h1) `compare` s2@(Solution i2 j2 h2)
    | area s1 > area s2 = GT
    | area s1 < area s2 = LT
    | i1 < i2 = GT
    | i1 > i2 = LT
    | j1 < j2 = GT
    | j1 > j2 = LT
    | otherwise = EQ

area :: Solution -> Int
area s = (end s - start s + 1) * (height s)

shift :: Int -> Solution -> Solution
shift x (Solution i j h) = Solution (i + x) (j + x) h

{-| For each cell, find the largest rectangle of the height of that cell.
    This precomputes the nearest index to the left and right of each cell
    with a value < the current cell.  This is used to efficiently look up
    the largest rectangle for that cell.  The precomputation takes O(n)
    and the routine here performs O(n) lookups.
-}
histArea :: [Int] -> Solution
histArea [] = Solution 0 0 0
histArea xs = maximum $ zipWith3 solve xs (nearestLT xs) (nearestGT xs)
  where n = length xs
        solve x lt gt = Solution i j x
          where i = 1 + fromMaybe (-1) lt
                j = (-1) + fromMaybe n gt

-- | Divide-and-conquer O(nlogn) solution.
histAreaDNQ :: [Int] -> Solution
histAreaDNQ [] = Solution 0 0 0
histAreaDNQ [x] = Solution 0 0 x
histAreaDNQ xs =
  let n = length xs
      m = n `div` 2
      (ls, rs) = splitAt m xs
      ls' = reverse ls
      lsol = histAreaDNQ ls
      rsol = shift m $ histAreaDNQ rs
      msols = helper (m - 1) m (min (head ls') (head rs)) (tail ls') (tail rs)
  in maximum $ lsol : rsol : msols
  where
    helper i j h [] [] = [Solution i j h]
    helper i j h [] (r:rs) =
      Solution i j h : helper i (j + 1) (min h r) [] rs
    helper i j h (l:ls) [] =
      Solution i j h : helper (i - 1) j (min h l) ls []
    helper i j h (l:ls) (r:rs)
      | l > r = Solution i j h : helper (i - 1) j (min h l) ls (r:rs)
      | otherwise = Solution i j h : helper i (j + 1) (min h r) (l:ls) rs

-- | Straightforward O(n^2) approach.
histAreaSane :: [Int] -> Solution
histAreaSane [] = Solution 0 0 0
histAreaSane xs = maximum $ concat $
                  zipWith4 helper [0..] [0..] xs (tail $ tails xs)
  where 
    helper i j h [] = [Solution i j h]
    helper i j h (x:xs) = Solution i j h : helper i (j + 1) (min h x) xs

isValid :: [Int] -> Solution -> Bool
isValid [] (Solution 0 0 0) = True
isValid xs (Solution i j h) =
  let n = length xs
  in  i >= 0 && i < n && j >= 0 && j < n && i <= j &&
      (all (>= h) $ take (j - i + 1) $ drop i xs)

-- | Given a sample input and expected value, test each approach.
testArea :: [Int] -> Int -> Test
testArea xs exp =
  let a = histAreaSane xs
      b = histAreaDNQ xs
      c = histArea xs
  in TestList [ area a ~?= exp
              , area b ~?= exp
              , area c ~?= exp
              , isValid xs a ~?= True
              , isValid xs b ~?= True
              , isValid xs c ~?= True
              ]

tests :: Test
tests = TestList
        [ testArea [1] 1
        , testArea [2] 2
        , testArea [1, 3] 3
        , testArea [3, 1] 3
        , testArea [2, 2] 4
        , testArea [2, 2, 2] 6
        , testArea [1, 3, 1] 3
        , testArea [2, 3, 2] 6
        , testArea [2, 3, 3, 2] 8
        , testArea [2, 4, 4, 2] 8
        , testArea [2, 5, 5, 2] 10
        , testArea [1, 3, 3, 1] 6
        , testArea [1, 3, 1, 1, 1] 5
        , testArea [5, 6, 7, 6, 5] 25
        , testArea [7, 6, 5, 6, 7] 25
        , testArea [1..5] 9
        , testArea [5,4..1] 9
        , testArea [1..6] 12
        , testArea [6,5..1] 12
        , testArea [1..7] 16
        , testArea [7,6..1] 16
        ]

data TestInput = TestInput [Int] deriving Show

instance Arbitrary TestInput where
  arbitrary = liftM TestInput $
              sized $ \n -> replicateM n (choose (0, n))

testSame :: TestInput -> Bool
testSame (TestInput xs) =
  let a = histAreaSane xs
      b = histAreaDNQ xs
      c = histArea xs
  in area a == area b && area b == area c &&
     isValid xs a && isValid xs b && isValid xs c

main :: IO ()
main = runTestTT tests >>
       quickCheckWith stdArgs { maxSuccess = 1000 } testSame

