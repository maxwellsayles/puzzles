{-
Find the smallest subsection of text that contains all search terms in order.

The first method scans the text from beginning to end. For each search
word encountered, we cons it with a chain that ends with the previous
search term, if one exists.  When the last search word is encountered, we
have a chain that represents the smallest subsection containing all
the terms in order that ends at the current location in the text. We keep
a running minimum of such chains.

TODO: Use a (Map Ix Chain), where Ix is the index into th search terms,
instead of a (Map String Chain) since when a word occurs more than once in the
search terms, we don't handle this correctly.

This runs in O(n).
-}

import Control.Applicative
import Data.Char
import Data.List
import Data.Maybe
import Data.Ord
import System.Environment

import qualified Data.Map as M

type WordIx = (String, Int)

data Chain = Chain { startIx :: Int,
                     links   :: [WordIx] }

-- O(n) length of chain links
lengthLinks :: [WordIx] -> Int
lengthLinks l = let end   = snd $ last l
                    start = snd $ head l
                in  end - start

-- O(1) length of chain links
lengthChain :: Chain -> Int
lengthChain c = let end   = snd . head . links $ c
                    start = startIx c
                in  end - start

singletonChain :: WordIx -> Chain
singletonChain (word, index) = Chain index [(word, index)]

consChain :: WordIx -> Chain -> Chain
consChain wix (Chain s c) = Chain s (wix:c)
                     
data Solution = Solution { bestChain :: Maybe Chain,
                           chains    :: M.Map String Chain }

lookupChain :: String -> Solution -> Maybe Chain
lookupChain w (Solution _ chains) = M.lookup w chains

insertChain :: String -> Chain -> Solution -> Solution
insertChain w c (Solution b cs) = Solution b (M.insert w c cs)

emptySolution :: Solution
emptySolution = Solution Nothing M.empty

-- Update a solution to contain the best chain given a new candidate chain.
updateSolution :: Maybe Chain -> Solution -> Solution
updateSolution Nothing s = s
updateSolution c (Solution Nothing chains) = Solution c chains
updateSolution c@(Just candidate) s@(Solution (Just best) chains) =
    let bl = lengthChain best
        cl = lengthChain candidate
    in  if bl <= cl then s else Solution c chains

search :: [String] -> [WordIx] -> Maybe [WordIx]
search searchWords textIndex =
    reverse . links <$>
    (bestChain $ foldl' helper emptySolution textIndex)
    where lastWord  = last searchWords
          firstWord = head searchWords
          prevWords = M.fromList $ zip (tail searchWords) searchWords

          helper :: Solution -> WordIx -> Solution
          helper acc wix@(word, index)
              | word == firstWord =
                  insertChain word (singletonChain wix) acc
              | word == lastWord  = 
                  updateSolution (lookupChain word res) res
              | otherwise = res
              where res = fromMaybe acc $ do
                            p <- M.lookup word prevWords
                            c <- lookupChain p acc
                            return $! insertChain word (consChain wix c) acc

searchStart :: [String] -> [WordIx] -> [WordIx] -> Maybe [WordIx]
searchStart [] _ acc = Just $ reverse acc
searchStart _ [] _   = Nothing
searchStart wss@(w:ws) (tix@(t,_):ts) acc
    | t == w    = searchStart ws ts (tix:acc)
    | otherwise = searchStart wss ts acc

slowSearch :: [String] -> [WordIx] -> [[WordIx]] -> Maybe [WordIx]
slowSearch _ [] [] = Nothing
slowSearch _ [] acc = Just $ minimumBy (comparing lengthLinks) acc
slowSearch ws ts acc = 
    let s = searchStart ws ts []
    in  case s of
          Nothing -> slowSearch [] [] acc
          Just l  -> slowSearch ws (tail $ dropWhile (/= head l) ts) (l:acc)

runIt :: FilePath -> [String] -> IO ()
runIt sourceFilePath searchWords = do
  text <- words .
          map toLower .
          filter (\c -> isAlpha c || isSpace c) <$>
          readFile sourceFilePath
  let textIndex = zip text [0..]

  print $ search searchWords textIndex
  print $ slowSearch searchWords textIndex []

usage :: IO ()
usage = do
  progName <- getProgName
  putStrLn $ "Usage: " ++ progName ++ " <canon.txt> <term1> [<term2> <term3> ...]"

main :: IO ()
main = do
  args <- getArgs
  if length args < 2
    then usage
    else runIt (head args) (tail args)
