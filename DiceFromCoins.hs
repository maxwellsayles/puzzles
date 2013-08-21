{-|
Given an infinite stream of coin flips uniformly at random,
generate an infinite stream of dice rolls uniformly at random.
Suppose you have to pay for each coin flipped.

One solution is to flip three coins to generate the values 0 through 7.
Values 0 through 5 represent the dice rolls 1 through 6.  Values 6 and
7 are thrown away and three more dice are rolled.  Unfortunately, this
solution is wasteful.

The solution here is a special case of arithmetic coding.  Imagine the
unit length is divided into six equal sized lengths representing the six
possible outcomes of a dice roll.  Each of these lengths are recursively
divided into six equal lengths and so on, each iteration representing the
outcome of subsequent dice rolls.  Two variables are maintained
indicating the range [x, y), and each coin flip divides the range in half.
Once both end points of the range are within a recursively defined
interval, a dice roll is determined and can be output.  Under this
interpretation, the range [x, y) is always getting smaller.

The implementation here normalizes recursive unit lengths to the interval
[0, 6).  Initially set x = 0 and y = 6.  A coin flip of 0 gives the range
[x, m) and a coin flip of 1 gives the range [m, y) where m = (x + y) / 2.
When x' = floor(x) and y' = ceil(y) are such that y' - x' = 1,
i.e. x and y are in the same integer range, then the coin flips have
determined the dice roll, x'.  At this point, the range is scaled by 6
and we recurse on the range [x'', y'') such that x'' = 6 * (x - x')
and y'' = 6 * (y - x').

Although no coin flips are wasted, the number of bits needed to
accurately represent each end point of the interval grow linearly with
the number of coin flips consumed.  As such, the space complexity is
linear with the input and output.

The output is a histogram of the dice values rolled.  Each quantity
should be roughly the same to indicate the uniformity of dice rolls.

NOTE: Since division is always by 2 and scaling is by 6, instead of using
a rational, we track the numerator and the value k for the denominator 2^k.
-}

import Control.Applicative
import Control.Arrow
import Data.Bits
import Data.List
import System.Random

{-| Converts an infinite lazy sequence of coin flips
    into an infinite lazy sequence of dice rolls." -}
dice :: [Int] -> [Int]
dice = f 0 6 0
    where f :: Integer -> Integer -> Int -> [Int] -> [Int]
          f x y k (b:bs)
              | v == (y - 1) `shiftR` k =
                  (fromInteger v) : f x' y' (k - 1) (b:bs)
              | b == 0 = f (x `shiftL` 1) m (k + 1) bs
              | b == 1 = f m (y `shiftL` 1) (k + 1) bs
              where m  = x + y
                    v  = x `shiftR` k
                    v' = v `shiftL` k
                    x' = 3 * (x - v')
                    y' = 3 * (y - v')

-- | Generate pairs (elem, count) representing
-- | the histogram of the input list.
histogram = map (head &&& length) . group . sort

gazillion = 10000

main = do
  xs <- randomRs (0,1) <$> newStdGen
  print $ histogram $ take gazillion $ dice xs

