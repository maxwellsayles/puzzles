import Data.Array.IArray (Array, array, listArray, (!))
import Data.List (inits, tails)
import Test.HUnit (Test, Test(TestList), (~?=), runTestTT)

{-| For balloon k, when it pops, what's the most we can earn? Depends what it's
next to. Just compute all possibilities, for n balloons, they can be next to n
balloons on th left, and n balloons on the right. This is O(n^3). The solution
is the most you can earn popping balloon k that is not next to anything.
|-}
solve :: [Int] -> Int
solve [] = 0
solve xs = opt ! (0, n')
  where
    n = length xs
    n' = n - 1

    xs' :: Array Int Int
    xs' = listArray (0, n') xs

    ranges = [(i, i + m - 1) | m <- [1 .. n], i <- [0 .. n - m]]

    opt :: Array (Int, Int) Int
    opt = array ((0, 0), (n', n')) [((i, j), helper i j) | (i, j) <- ranges]

    lookupVal i
      | i < 0 || i >= n = 1
      | otherwise = xs' ! i

    lookupOpt i j
      | j < i = 0
      | otherwise = opt ! (i, j)

    helper i j = maximum $ map val [i..j]
      where
        i' = lookupVal (i - 1)
        j' = lookupVal (j + 1)
        val k = let k' = xs' ! k
                in (i' * j' * k') + lookupOpt i (k - 1) + lookupOpt (k + 1) j

{-| Intentionally slow, reference implementation. This tries all permutations 
and then takes the maximum. |-}
solvePermute :: (Ord a, Num a) => [a] -> a
solvePermute [] = 0
solvePermute xs =
  maximum $
  map (\((x, y, z), xs') -> x * y * z + solvePermute xs') $
  zip triples missing
  where
    triples = zip3 (1:xs) xs (tail xs ++ [1])
    missing = zipWith (++) (inits xs) (tail $ tails xs)

testFn :: ([Int] -> Int) -> Test
testFn f = TestList
  [ f [] ~?= 0
  , f [1] ~?= 1
  , f [2] ~?= 2
  , f [1, 2] ~?= 4
  , f [1, 2, 3] ~?= 12
  , f [-1, 2, 3] ~?= 7
  , f [-1, 3, 2] ~?= 6
  , f [-1, -2, 3] ~?= 6
  , f [-1, -2, -3] ~?= 7
  ]

main :: IO ()
main = do
  _ <- runTestTT (testFn solve)
  _ <- runTestTT (testFn solvePermute)
  return ()
