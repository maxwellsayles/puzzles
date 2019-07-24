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
