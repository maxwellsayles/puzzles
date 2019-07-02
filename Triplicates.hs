{-| Given an array where each input occurs three times, except for one value
which only occurs once, find the value that only occurs once. |-}

import Control.Applicative
import Control.Exception (assert)
import Control.Monad
import Data.Bits (Bits, (.&.), complement, xor)
import Data.List (sort)
import Test.HUnit hiding (assert)
import Test.QuickCheck hiding ((.&.))

data TestInput = TestInput [Int] deriving Show

-- This is the O(n) time and O(1) space solution, using bitwise addition mod 3.
uniq :: (Foldable t, Bits a, Num a) => t a -> a
uniq xs = let (x1, x0) = foldr bandMod3 (0, 0) xs
          in assert (x1 == 0) x0
  where bandMod3 y (x1, x0) =
          let x0' = x0 `xor` y
              x1' = x1 `xor` (x0 .&. y)
          in (x1' .&. complement x0', x0' .&. complement x1')

-- This is the O(nlogn) time and O(n) space solution, using sorting.
uniqBySorting :: Ord a => [a] -> a
uniqBySorting xs =
  let [x] = helper $ sort xs in x
  where helper [] = []
        helper [x] = [x]
        helper (x1:x2:x3:xs)
          | x1 == x2 = assert (x2 == x3) $
                       helper xs
          | otherwise = x1 : helper (x2:x3:xs)

instance Arbitrary TestInput where
  arbitrary =
    liftM TestInput $
    sized $ \n -> do
      xs <- concat <$> replicateM n (arbitrary >>= \x -> return [x, x, x])
      x <- arbitrary
      shuffle (x : xs)

testIt :: TestInput -> Bool
testIt (TestInput xs) =
  uniq xs == uniqBySorting xs

tests :: Test
tests = TestList
        [ (uniq [] :: Int) ~?= 0
        , (uniq [1] :: Int) ~?= 1
        , (uniq [1, 2, 1, 1] :: Int) ~?= 2
        , (uniq [3, 2, 1, 2, 1, 2, 1] :: Int) ~?= 3
        , (uniq [1, 2, 1, 2, 1, 2, 3] :: Int) ~?= 3
        , (uniq [1, 1, 1, 2, 2, 2, 1] :: Int) ~?= 1
        ]

main :: IO ()
main = do
  runTestTT tests
  quickCheckWith (stdArgs { maxSuccess = 1000, maxSize = 100 }) testIt
