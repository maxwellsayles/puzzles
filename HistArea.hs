-- TODO: Verify solution fits within histogram

import Control.Monad
import Data.Array.IArray
import Data.List
import Data.Maybe
import Data.Ord
import Test.HUnit
import Test.QuickCheck

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
                         } deriving Show

area :: Solution -> Int
area s = (end s - start s + 1) * (height s)

shift :: Int -> Solution -> Solution
shift x (Solution i j h) = Solution (i + x) (j + x) h

histArea :: [Int] -> Solution
histArea [] = Solution 0 0 0
histArea xs = maximumBy (comparing area) $
              zipWith3 solve xs (nearestLT xs) (nearestGT xs)
  where n = length xs
        solve x lt gt = Solution i j x
          where i = 1 + fromMaybe (-1) lt
                j = (-1) + fromMaybe n gt

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
  in maximumBy (comparing area) $ lsol : rsol : msols
  where
    helper i j h [] [] = [Solution i j h]
    helper i j h [] (r:rs) =
      Solution i j h : helper i (j + 1) (min h r) [] rs
    helper i j h (l:ls) [] =
      Solution i j h : helper (i - 1) j (min h l) ls []
    helper i j h (l:ls) (r:rs)
      | l > r = Solution i j h : helper (i - 1) j (min h l) ls (r:rs)
      | otherwise = Solution i j h : helper i (j + 1) (min h r) (l:ls) rs

histAreaSane :: [Int] -> Solution
histAreaSane [] = Solution 0 0 0
histAreaSane xs = maximumBy (comparing area) $
                  concat $
                  zipWith4 helper [0..] [0..] xs (tail $ tails xs)
  where 
    helper i j h [] = [Solution i j h]
    helper i j h (x:xs) = Solution i j h : helper i (j + 1) (min h x) xs
  
testArea :: [Int] -> Int -> Test
testArea xs exp = TestList
           [ area (histAreaSane xs) ~?= exp
           , area (histAreaDNQ xs) ~?= exp
           , area (histArea xs) ~?= exp
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
  let a = area $ histAreaSane xs
      b = area $ histAreaDNQ xs
      c = area $ histArea xs
  in a == b && b == c

main :: IO ()
main = runTestTT tests >>
       quickCheckWith stdArgs { maxSuccess = 1000 } testSame

