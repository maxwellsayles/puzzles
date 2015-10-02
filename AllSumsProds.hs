{- Input is an arithmetic string of whole numbers interleaved with + or *.
Without assuming any precendence between + and *, compute all possible values
generated by insertion of parentheses.  E.g.

"1 + 2 * 3 + 4" could be parenthesized as "(1 + 2) * (3 + 4)" or
"((1 + 2) * 3) + 4".

The solution here is to split the input at each index into left and right
halves.  Compute all possible values generated for each half, and then either
add or multiple each pair generated from an element from the left and right
halves.

The runtime of this is likely O(n!).  The solution could take advantage of
memoization, but the output size empirically appears to be at least O(3^n),
which is a great improvement over O(n!), but I'm not sure how to prove that
it's not larger than O(3^n).
-}

import Control.Applicative
import Data.List (inits, tails)
import Data.Set (Set)
import qualified Data.Set as Set
import Test.HUnit

data Op = Add | Mul

allVals :: [Int] -> [Op] -> Set Int
allVals [x] [] = Set.singleton x
allVals xs ops =
  Set.fromList $ concat $ map Set.toList $ zipWith3 apply ls rs ops
  where
    -- All possible left hand sides
    xsl = init $ tail $ inits xs
    opsl = init $ inits ops
    ls = zipWith allVals xsl opsl

    -- All possible right hand sides
    xsr = init $ tail $ tails xs
    opsr = tail $ tails ops
    rs = zipWith allVals xsr opsr

    opf Add = (+)
    opf Mul = (*)

    -- Apply op to all pairs from left and right
    apply ls rs op =
      let ls' = Set.toList ls
          rs' = Set.toList rs
      in Set.fromList $ opf op <$> ls' <*> rs'

testIt :: [Int] -> [Op] -> [Int] -> Test
testIt xs ops res = allVals xs ops ~?= Set.fromList res

testSingle :: Test
testSingle = testIt [1] [] [1]

testAddOnly :: Test
testAddOnly = TestList
  [ testIt [1, 2] [Add] [3]
  , testIt [1, 2, 3] [Add, Add] [6]
  , testIt [1, 2, 3, 4] [Add, Add, Add] [10]
  ]

testAddMul :: Test
testAddMul = testIt [1, 2, 3] [Add, Mul] [9, 7]

testMulOnly :: Test
testMulOnly = TestList
  [ testIt [1, 2] [Mul] [2]
  , testIt [1, 2, 3] [Mul, Mul] [6]
  , testIt [1, 2, 3, 4] [Mul, Mul, Mul] [24]
  ]

testAddMulAdd :: Test
testAddMulAdd = testIt [1, 2, 3, 4] [Add, Mul, Add] [11, 15, 21, 13]

testPrimes :: Test
testPrimes =
  let res = allVals [2, 3, 5, 7, 11, 13, 17, 19, 23, 29]
                    [Add, Mul, Add, Mul, Add, Mul, Add, Mul, Add]
  in Set.size res ~?= 1354

tests :: Test
tests = TestList
  [ testSingle
  , testAddOnly
  , testAddMul
  , testMulOnly
  , testAddMulAdd
  , testPrimes
  ]

main :: IO ()
main = runTestTT tests >> return ()
