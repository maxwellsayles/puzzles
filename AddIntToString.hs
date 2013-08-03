{-
It solves the following question: You are given a string representation of
an arbitrarily large integer (could be gigabytes) and a 64-bit integer.
Add them together and produce the new string.

It treats the input Int as the carry. You add the carry to the right most
digit and compute both a new digit and a new carry (x' and carry'
respectively).

The problem is that two things are computed, so using a fold is tricky.

The accumulator in the fold is a function from Int -> String -> String. 
That's the k parameter and the fourth and fifth arguments to foldl
reduce the result to a string.

Suppose the accumulator were a function from Int -> String instead. 
After computing the new carry', we run the carry' through the left of
the computation using (k carry').  Since the carry moves left, the
current character would be appended to the result, which would look
like (k carry' ++ [x']), but this would take O(n^2) for n appends.

Instead, we use difference lists.  That's where the Int -> String -> String
comes in.  Lists are represented as functions and we only need to
concatenate functions which takes O(n) for n concatenations. 
-}

import Control.Applicative ((<$>))
import Control.Monad.State
import Data.Char (digitToInt, intToDigit)
import Data.Foldable (foldrM)
import System.Random

addCont :: String -> Int -> String
addCont s i = foldl f g s i ""
    where g 0     = id
          g carry = (show carry ++)
          f k x carry =
              let y      = digitToInt x + carry `mod` 10
                  carry' = carry `div` 10 + y `div` 10
                  x'     = intToDigit $ y `mod` 10
              in  k carry' . (x':)

addState :: String -> Int -> String
addState s i =
    let (s', carry) = runState (foldrM f "" s) i
    in  if carry /= 0 then show carry ++ s' else s'
    where f x acc = do
            carry <- get
            let y      = digitToInt x + carry `mod` 10
            let carry' = (carry `div` 10) + (y `div` 10)
            let x'     = intToDigit $ y `mod` 10
            put carry'
            return $ x' : acc
            
add :: String -> Int -> String
add s i = reverse $ helper (reverse s) i
    where helper []     0     = ""
          helper []     carry = reverse $ show carry
          helper (x:xs) carry =
              let y      = digitToInt x + carry `mod` 10
                  carry' = (carry `div` 10) + (y `div` 10)
                  x'     = intToDigit $ y `mod` 10
              in  x' : helper xs carry'

main = do
  x <- show <$> randomRIO ((2^199)::Integer, 2^200)
  y <- randomRIO (0, maxBound :: Int)
  let z1 = add x y
  let z2 = addState x y
  let z3 = addCont x y
  let z4 = show $ (read x :: Integer) + fromIntegral y
  putStrLn z1
  putStrLn z2
  putStrLn z3
  print $ z1 == z2 && z2 == z3 && z3 == z4
  putStrLn $ addCont "99" 10000
  
