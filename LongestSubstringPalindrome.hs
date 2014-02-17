{-|
A palindrome is a string that is the same forward and back.

We implement two algorithms to compute the longest substring palindrome.

For both algorithms, we first translate a string "asdfdsa"
into "a s d f d s a" and a string "asdffdsa" into "a s d f f d s a".
This makes it straight forward to handle both types of palindromes.

The first algorithm iterates each index i in the string and expands around i
to find the largest palindrome.  This requires O(n^2) time.

The second algorithm using memoization.  At each index i we compute
the largest palindrome centered at index i, as well as the largest
palindrome centered at or before i such that the palindrome includes
index i.  Let c be the largest palindrome centered at c <= i that
includes index i.  Then the largest palindrome centered at i is either
the largest palindrome centered at c - (i - c), or we expand the palindrome
centered at index i.  Using the largest palindrome centered at c - (i - c)
uses the solution mirrored around c and is guaranteed to include index i,
by definition.

For additional explanation, see:
http://leetcode.com/2011/11/longest-palindromic-substring-part-i.html

-}

import Control.Monad (forM)
import Data.Array.IArray (Array, bounds, elems, listArray, (!))
import Data.Ix (range)
import Data.List (intersperse, maximumBy)
import Data.Ord (comparing)
import Test.QuickCheck

-- | The internal representation of "asdffdsa" is "a s d f f d s a".
-- | this allows the algorithm to center between 'f'.
toIntern :: String -> Array Int Char
toIntern xs = let xs' = intersperse ' ' xs
                  n   = length xs'
              in  listArray (1, n) xs'

-- | Given an internal representation, a center and a diameter,
-- | generate a string of the odd indices.
toExtern :: Array Int Char -> Int -> Int -> String
toExtern arr i d = map (arr !) $ filter odd $ range (i - d, i + d)

-- | Find the largest palindrome centered around index i.
expand :: Array Int Char -> Int -> Int -> Int
expand arr i d
    | i - d < l = d - 1
    | i + d > u = d - 1
    | x /= y    = d - 1
    | otherwise = expand arr i (d + 1)
    where x = arr ! (i - d)
          y = arr ! (i + d)
          (l, u) = bounds arr


-- | Uses an O(n^2) time algorithm.
sanity :: String -> String
sanity "" = ""
sanity xs =
    let arr    = toIntern xs
        ixs    = range $ bounds arr
        ds     = map (\i -> expand arr i 1) ixs
        (i, d) = maximumBy (comparing snd) $ zip ixs ds
    in  toExtern arr i d

-- | The O(n) time algorithm.
longest :: String -> String
longest "" = ""
longest xs =
    let ds     = map snd $ elems opt
        (i, d) = maximumBy (comparing snd) $ zip [l..] ds
    in  toExtern arr i d
    where
      arr    = toIntern xs
      (l, u) = bounds arr

      opt :: Array Int ((Int, Int), Int)
      opt = listArray (l, u) $ base : map step [(l + 1)..]

      base = ((l, 0), 0)

      step i
          | i > c + d     = let d' = expand arr i 1
                            in  ((i, d'), d')
          | i + x < c + d = ((c, d), x)
          | otherwise     = let d' = expand arr i (c + d - i + 1)
                            in  ((i, d'), d')
          where (c, d) = fst $ opt ! (i - 1)
                x      = snd $ opt ! (2 * c - i)

{- | Verify that the palindrome computed using the O(n) method
   is the same length as the palindrome computed using the O(n^2)
   and that both are actually palindromes. -}
verify (TestString s) =
    let p1 = longest s
        p2 = sanity s
    in  length p1 == length p2 &&
        p1 == reverse p1 &&
        p2 == reverse p2
    
test = do
  let args = stdArgs { maxSuccess = 100, maxSize = 10000 }
  quickCheckWith args verify

data TestString = TestString String deriving Show

{- | Generate an arbitrary string prefix, suffix, and infix where
   the infix is appended with its reverse to force an interior large
   palindrome.  The prefix and suffix can be arbitrary sized strings
   including the empty string for testing the palindrome at the start
   and end of the string. -}
instance Arbitrary TestString where
    arbitrary = sized $ \n -> do
                  prefix <- arbitrary
                  suffix <- arbitrary
                  n' <- choose (0, n)
                  xs <- sequence $ replicate n' arbitrary
                  let xs' = xs ++ reverse xs
                  return $ TestString (prefix ++ xs' ++ suffix)

main = test
