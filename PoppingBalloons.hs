import Data.List (inits, tails)
import Data.Array.IArray (Array, (!), listArray)
import Test.HUnit

solvePermute :: (Ord a, Num a) => [a] -> a
solvePermute [] = 0
solvePermute xs =
  maximum $
  map (\((x, y, z), xs') -> x * y * z + solvePermute xs') $
  zip triples missing
  where
    triples = zip3 (1:xs) xs (tail xs ++ [1])
    missing = zipWith (++) (inits xs) (tail $ tails xs)

tests :: Test
tests = TestList
  [ solvePermute [] ~?= 0
  , solvePermute [1] ~?= 1
  , solvePermute [2] ~?= 2
  , solvePermute [1, 2] ~?= 4
  , solvePermute [1, 2, 3] ~?= 12
  , solvePermute [-1, 2, 3] ~?= 7
  , solvePermute [-1, 3, 2] ~?= 6
  ]

main :: IO ()
main = do
  _ <- runTestTT tests
  return ()
