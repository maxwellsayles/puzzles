{-# LANGUAGE BangPatterns #-}

{-
Given an array where every element is repeated twice except for exactly two
elements that appear only once, find those two elements.

The xor of all elements gives a mask where the set bits are where the two
elements differ. Since they are not the same, there is at least one bit where
they differ. Find the smallest index of that bit, then partition the list
based on elements where the bit is set/clear and use the xor trick.

This is O(n) time and O(1) space.
-}

import Control.Exception (assert)
import Control.Monad (liftM)
import Data.Bits
import Test.HUnit (Test( TestList ), runTestTT, (~?=))
import Test.QuickCheck hiding ((.&.))

lsb :: Int -> Int
lsb 0 = assert False 0
lsb x = loop x 0
  where
    loop x !acc =
      if x .&. 1 == 1 then
        acc
      else
        loop (x `shiftR` 1) (acc + 1)

mask :: [Int] -> Int
mask xs = loop xs 0
  where
    loop [] !acc = acc
    loop (x:xs) !acc = loop xs (acc `xor` x)

solve :: [Int] -> (Int, Int)
solve xs = loop xs 0 0
  where
    m = 1 `shiftL` (lsb $ mask xs)
    loop [] !y !z =
      if y < z then (y, z) else (z, y)
    loop (x:xs) !y !z
      | x .&. m /= 0 = loop xs (x `xor` y) z
      | otherwise = loop xs y (x `xor` z)

tests :: Test
tests = TestList
    [ solve [1, 2] ~?= (1, 2)
    , solve [2, 3] ~?= (2, 3)
    , solve [1, 1, 2, 3] ~?= (2, 3)
    , solve [2, 3, 1, 1] ~?= (2, 3)
    , solve [1, 1, 2, 2, 3, 4] ~?= (3, 4)
    , solve [0, 1, 1, 2, 2, 3, 3, 4, 4, 5] ~?= (0, 5)
    ]

data TestCase =
  TestCase [Int] (Int, Int)
  deriving Show

instance Arbitrary TestCase where
  arbitrary = do
    xs <- arbitrary
    y <- arbitrary
    z <- suchThat arbitrary (\z -> z /= y)
    xs' <- shuffle (xs ++ xs ++ [y, z])
    let v = if y < z then (y, z) else (z, y)
    return $! TestCase xs' v

verify :: TestCase -> Bool
verify (TestCase xs v) =
  solve xs == v

main :: IO ()
main = do
  runTestTT tests >> return ()
  quickCheckWith stdArgs { maxSuccess = 10000 } verify
