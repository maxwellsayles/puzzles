{-| This simulates an unbiased coin given a biased coin. The protocol is simple.
Flip the biased coin twice. If it's the same value, start over. Otherwise,
return the value of the first flip. The intuition behind why this works is that
when two flips are different, the position of the biased/unbiased value are equally
likely.
|-}

import System.Random

biased :: StdGen -> [Bool]
biased = map (\x -> x > 7) . randomRs (1 :: Int, 10)

unbiased :: [Bool] -> [Bool]
unbiased (x:y:xs)
  | x == y = unbiased xs
  | otherwise = x : unbiased xs

main :: IO ()
main = do
  gen <- newStdGen
  print $ length $ filter id $ take 100000 $ unbiased $ biased gen
