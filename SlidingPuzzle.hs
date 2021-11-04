{-
8 Puzzle.

Not every initial configuration is solvable!
-}

{-# LANGUAGE RecordWildCards #-}

import Data.Hashable
import Data.List (intercalate)
import Test.QuickCheck
import Text.Printf (printf)

import qualified Data.Map as M
import qualified Data.HashSet as HS

data PuzzleState = PuzzleState
  { posNum :: M.Map (Int, Int) Int
  , emptyPos :: (Int, Int)
  } deriving (Eq)

instance Show PuzzleState where
  show (PuzzleState {..}) =
    let lookup x y
          | (x, y) == emptyPos = '.'
          | otherwise = head $ show $ posNum M.! (x, y)
    in intercalate "\n" $
       [ [lookup x y | x <- [1..3]] | y <- [1..3] ]

instance Arbitrary PuzzleState where
  arbitrary = do
    pos' <- shuffle [(x, y) | x <- [1..3], y <- [1..3]]
    let pos = M.fromList $ zip pos' [1..8]
    return $ PuzzleState pos (last pos')

instance Hashable PuzzleState where
  hashWithSalt salt (PuzzleState {..}) =
    hashWithSalt salt (M.toList posNum, emptyPos)

isSolved :: PuzzleState -> Bool
isSolved (PuzzleState {..}) =
  emptyPos == (3, 3) &&
  let solvedPosNum =
        M.fromList $ zip [(x, y) | y <- [1..3], x <- [1..3]] [1..8]
  in posNum == solvedPosNum

{- Swap the empty position with a number. -}
move :: Int -> Int -> PuzzleState -> PuzzleState
move x y ps@(PuzzleState {..})
  | isInvalidMove x y ps =
      error $ printf "Invalid move to %d %d\n%s" x y (show ps)

  | otherwise =
      let num = posNum M.! (x, y)
          posNum' = M.insert (emptyPosX, emptyPosY) num $
                    M.delete (x, y) $
                    posNum
      in PuzzleState { posNum = posNum'
                     , emptyPos = (x, y)
                     }
  where
    (emptyPosX, emptyPosY) = emptyPos

allPos :: [(Int, Int)]
allPos = [(x, y) | y <- [1..3], x <- [1..3]]

possibleMoves :: PuzzleState -> [PuzzleState]
possibleMoves ps =
  let validPos = filter (\(x, y) -> isValidMove x y ps) allPos
  in map (\(x, y) -> move x y ps) validPos

isValidMove :: Int -> Int -> PuzzleState -> Bool
isValidMove x y ps = not (isInvalidMove x y ps)

isInvalidMove :: Int -> Int -> PuzzleState -> Bool
isInvalidMove x y (PuzzleState _ (emptyPosX, emptyPosY))
  | x /= emptyPosX && y /= emptyPosY = True
  | x == emptyPosX && y == emptyPosY = True
  | x < 1 || x > 3 || y < 1 || y > 3 = True
  | abs (emptyPosX - x) > 1 = True
  | abs (emptyPosY - y) > 1 = True
  | otherwise = False

solve :: HS.HashSet PuzzleState -> [PuzzleState] -> PuzzleState
solve visited (ps:pss)
  | isSolved ps = ps
  | HS.member ps visited = solve visited pss
  | otherwise =
      let states' = pss ++ possibleMoves ps
          visited' = HS.insert ps visited
      in solve visited' states'

main :: IO ()
main = do
  ps <- generate (arbitrary :: Gen PuzzleState)
  print ps
  let solved = solve HS.empty [ps]
  print solved
