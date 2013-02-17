{-
Given an unsorted array of integers, the output should be such that
A[0] <= A[1] >= A[2] <= A[3] >= A[4]....

The solution is linear in the input.  We first expect the lead pair (x,y)
to be _ <= _.  If x <= y, we output x as the head, and recurse on (y:ys)
and we reverse the expected relationship. If x >= y, then we output y and
recurse on (x:ys).  

To see that this works, consider the triple (x, y, z), and assume that
we already output x.  If we expected x <= _, then this implies that
x <= y (since otherwise we would have output y instead of x).  The
next pair should be _ >= _.  If y >= z, then we output (x:y:_) and we
have x <= y >= _ (which may be z). If y <= z, then we output (x:z:_).
Since x <= y <= z, we have x <= z >= _ (which may be y). A similar argument
applies when we expected x >= _ for the initial relationship of the triple.
-}
import System.Random

wiggle :: Ord a => Ordering -> [a] -> [a]
wiggle _ [] = []
wiggle _ [x] = [x]
wiggle GT (x:y:ys)
    | x >= y    = x : wiggle LT (y:ys)
    | otherwise = y : wiggle LT (x:ys)
wiggle LT (x:y:ys)
    | x <= y    = x : wiggle GT (y:ys)
    | otherwise = y : wiggle GT (x:ys)

verify :: Ord a => Ordering -> [a] -> Bool
verify _ [] = True
verify _ [x] = True
verify GT (x:y:ys)
    | x >= y    = verify LT (y:ys)
    | otherwise = False
verify LT (x:y:ys)
    | x <= y    = verify GT (y:ys)
    | otherwise = False

main = do
  -- This never prints, since we use an infinite list and the test
  -- never fails.  All we can do is watch the machine heat up for a bit.
  print . verify LT . wiggle LT . randomRs (1,100::Int) =<< newStdGen