-- | See BallSwitchBoard.java for an explanation.

import Data.Array.IArray (listArray, bounds, (!), Array)
import Data.Array.Unboxed (UArray)

-- -1 => leaves through the bottom
-- +1 => leaves through the right
ballSwitchBoard board k = above width (height+1) 
  where 
    (width, height) = snd $ bounds board

    lefts :: Array (Int,Int) Int
    lefts = listArray (bounds board)
              [left x y | x <- [1..width], y <- [1..height]]

    aboves :: Array (Int,Int) Int
    aboves = listArray (bounds board)
               [above x y | x <- [1..width], y <- [1..height]]


    -- total number of balls that enter a cell
    total loc = aboves ! loc + lefts ! loc

    -- the number of balls that enter from above
    above 1 1 = k
    above x 1 = 0
    above x y 
      | cell == -1 = ((total loc)+1) `div` 2
      | cell ==  0 = aboves ! loc
      | cell ==  1 = (total loc) `div` 2
      where loc = (x,y-1)
            cell = board ! loc

    -- the number of balls that enter from the left
    left 1 y = 0
    left x y
      | cell == -1 = (total loc) `div` 2
      | cell ==  0 = lefts ! loc
      | cell ==  1 = ((total loc)+1) `div` 2
      where loc = (x-1,y)
            cell = board ! loc


main = do
  let board = listArray ((1,1), (3,2)) 
                        [-1, 1, 0, 0, -1, 0] :: UArray (Int,Int) Int
  print $ ballSwitchBoard board 4