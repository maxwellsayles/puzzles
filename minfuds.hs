{-
This problem was from codility.com in March 2010.

See minfuds.py for more explanation.
-}

import Control.Monad
import Data.Function
import Data.List
import qualified Data.Set as Set
import System.Random

-- State Monad
newtype State s a = State { runState :: s -> (a, s) }

instance Monad (State s) where
    return a = State $ \s -> (a, s)
    m >>= f = State $ \s ->
              let (a, s') = runState m s
              in  runState (f a) s'

nRandomR :: (Random a, RandomGen s) => Int -> (a, a) -> s -> ([a], s)
nRandomR n r = runState . sequence . replicate n $ (State $ randomR r)
    
-- a line is defined as y = m*x + b
data Line a = Line { getM :: a, 
                     getB :: a 
                   }
    deriving (Show, Eq, Ord)


-- given a x coordinate and a line, compute the y value
lineY :: Num a => a -> Line a -> a
lineY x (Line m b) = m*x + b


-- returns the X coordinate of the intersection of two lines
-- a line defined as mx+b=y intersects as:
-- m_1*x + b_1 = m_2*x + b_2
-- => x = (b_2 - b_1) / (m_1 - m_2) 
intersectX :: Fractional a => Line a -> Line a -> a
intersectX (Line m1 b1) (Line m2 b2) =
    (b2 - b1) / (m1 - m2)


minfuds :: (Fractional a, Ord a) => [a] -> [a] -> a
minfuds as bs =
    let ls = sort $ zipWith Line as bs
        upper = curve $ reverse ls
        lower = curve ls
        (_, top, _) = head upper
        (_, bottom, _) = head lower

    in minimum .
       map (\(x, a, b) -> lineY x a - lineY x b) $
       merge top bottom upper lower

    where curve = convexity . 
                  reverse . 
                  map head .
                  groupBy ((==) `on` getM)

          merge top bottom u@((ux, ul1, ul2):uppers) l@((lx, ll1, ll2):lowers) =
              if ux < lx
              then (ux, ul2, bottom) : merge ul2 bottom uppers l
              else (lx, top, ll2) : merge top ll2 u lowers
          merge top bottom ((ux, ul1, ul2):uppers) _  =
              (ux, ul2, bottom) : merge ul2 bottom uppers []
          merge top bottom _ ((lx, ll1, ll2):lowers) =
              (lx, top, ll2) : merge top ll2 [] lowers
          merge _ _ _ _ = []

-- compute the lines that make the convex surface
-- lines given as input should be in sorted order by slope
convexity :: (Fractional a, Ord a) => [Line a] -> [(a, Line a, Line a)]
convexity ls = reverse . foldl' f [] $ tail ls
    where f [] l = [(intersectX l $ head ls, head ls, l)]
          f curve@((frontX, _, frontL):tl) l =
              let x = intersectX l frontL
              in  if x > frontX
                  then (x, frontL, l):curve
                  else f tl l


-- Compute all line intersections, O(n^2),
-- and the minimum/maximum y value for all lines, O(n).
-- Take the minimum of the differences. Total is O(n^3).
sanity :: (Fractional a, Ord a) => [a] -> [a] -> a
sanity as bs =
    let ls = zipWith Line as bs
        xs = Set.toList . Set.fromList . 
             map (uncurry intersectX) .
             filter (\(x, y) -> x /= y) $ 
             liftM2 (,) ls ls
        ys x = map (lineY x) ls
        ds = map (\x -> let ts = ys x in maximum ts - minimum ts) xs
    in  minimum ds
        

main :: IO ()
main = do
  let n = 1024
  (as, s') <- return . nRandomR n (-1000::Double, 1000) =<< newStdGen
  (bs,  _) <- return . nRandomR n (-1000::Double, 1000) $ s'
  print $ minfuds as bs
  print $ sanity as bs
