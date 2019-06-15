import Control.Applicative
import Data.List (scanl')
import System.Random

data Point = Point Double Double
  deriving Show

instance Random Point where
  randomR (Point x1 y1, Point x2 y2) g =
    let (x, g') = randomR (x1, x2) g
        (y, g'') = randomR (y1, y2) g'
    in (Point x y, g'')

  random g =
    let (x, g') = random g
        (y, g'') = random g'
    in (Point x y, g'')

isInUnit :: Point -> Bool
isInUnit (Point x y) = x * x + y * y <= 1

step :: (Int, Int) -> Point -> (Int, Int)
step (num, den) p =
  (num + if isInUnit p then 1 else 0, den + 1)

unitRPoints :: IO [Point]
unitRPoints = randomRs (Point 0.0 0.0, Point 1.0 1.0) <$> newStdGen

everyNth :: Int -> [a] -> [a]
everyNth n xs = head xs : everyNth n (drop (n - 1) xs)

main :: IO ()
main = do
  ps <- unitRPoints
  let series = scanl' step (0, 0) ps
  mapM_ print $
    map (\(num, den) -> 4 * fromIntegral num / fromIntegral den) $
    everyNth 100000 $
    tail series
