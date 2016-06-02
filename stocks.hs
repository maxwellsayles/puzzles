{- |
Can buy/sell many times, can only hold at most 1 share, at most one transaction
per day.

Two approaches.  The first maintains the paper value each day where we either hold
stock A, hold stock B, or hold nothing.

The second approach keeps the actual profit, and again at the end of the day
we either own A, own B, or nothing.
-}

import Control.Monad
import Data.List
import Test.QuickCheck

stocks1 :: (Num a, Ord a) => [a] -> [a] -> a  
stocks1 [] [] = 0
stocks1 as bs = (\(_, _, z) -> z) $ foldl' step (0, 0, 0) $ zip das dbs
  where das = zipWith (-) (tail as) as
        dbs = zipWith (-) (tail bs) bs
        step (x, y, z) (da, db) = (x', y', z')
          where x' = max (x + da) z
                y' = max (y + db) z
                z' = max x' y'

stocks2 :: (Num a, Ord a) => [a] -> [a] -> a
stocks2 [] [] = 0
stocks2 (a:as) (b:bs) = (\(_, _, z) -> z) $ foldl' step (-a, -b, 0) $ zip as bs
  where step (x, y, z) (a, b) = (x', y', z')
          where x' = max x (z - a)
                y' = max y (z - b)
                z' = maximum [z, x + a, y + b]

-- NOTE: Unbounded since Int can overflow
data TestInput = TestInput ([Integer], [Integer]) deriving Show

instance Arbitrary TestInput where
  arbitrary = sized $ \n -> do
    xs <- replicateM n arbitrary
    ys <- replicateM n arbitrary
    return $ TestInput (xs, ys)

verify :: TestInput -> Bool
verify (TestInput (xs, ys)) = stocks1 xs ys == stocks2 xs ys

main :: IO ()
main = quickCheckWith (stdArgs { maxSuccess = 1000, maxSize = 10000 }) verify

               
