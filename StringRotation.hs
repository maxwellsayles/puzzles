import Control.Monad (forM_)
import Data.Array (listArray, (!))
import System.Random (newStdGen, randomRs, randomR)
import Test.HUnit

{-
Determines if one string is a rotation of the other in O(n).

i and j are starting indexes into the strings a and b.

k is the relative index of the character under consideration.
if the characters match, k is incremented.
if the characters don't match, the string with the 
lesser character has it's initial index advanced to the
following character.

Once n matches occur, we return True.
If i or j go past n, the strings are not a rotation of each other.
-}
isRotation a b
  | length a /= length b = Nothing
  | otherwise = loop 0 0 0
  where
    n = length a
    a' = listArray (0, n-1) a
    b' = listArray (0, n-1) b
    loop i j k
      | k == n = Just ((j-i) `mod` n)
      | i >= n = Nothing
      | j >= n = Nothing
      | i' == j' = loop i j (k+1)
      | i' < j' = loop (i+k+1) j 0
      | otherwise = loop i (j+k+1) 0
      where
        i' = a' ! ((i+k) `mod` n)
        j' = b' ! ((j+k) `mod` n)

testRandom :: Int -> Test
testRandom n = TestCase $ do
  gen <- newStdGen
  let a = take n $ (randomRs ('a','z') gen) :: String
      (s, _) = randomR (1, n) gen
      b = (drop s a) ++ (take s a)
  assertBool (a ++ " " ++ b) $ isRotation a b == Just (n - s)

tests :: Test
tests = TestList $
        [ isRotation "" "a" ~?= Nothing
        , isRotation "a" "a" ~?= Just 0
        , isRotation "ab" "ba" ~?= Just 1
        , isRotation "..." "..." ~?= Just 0
        , isRotation "abababc" "ababcab" ~?= Just 5
        , isRotation "ababcab" "abababc" ~?= Just 2
        , isRotation "abababc" "abababd" ~?= Nothing
        , isRotation "HELLOHELLOHELLO1HELLOHELLOHELLO2"
          "HELLOHELLOHELLO2HELLOHELLOHELLO1" ~?= Just 16
        ] ++ [testRandom n | n <- [1..1000]]

main :: IO ()
main = runTestTT tests >> return ()
