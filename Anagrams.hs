{- This one speaks for itself -}
import Control.Applicative
import Control.Arrow
import Control.Monad
import Data.Char
import Data.Function
import Data.List
import Data.Ord
import qualified Data.Set as Set

longestAnagrams :: [String] -> [[String]]
longestAnagrams =
  reverse .
  sortBy (comparing (length . head)) .
  filter ((>1) . length) .
  map (map fst) .
  groupBy ((==) `on` snd) .
  sortBy (comparing snd) .
  map (id &&& sort) .
  Set.toList . Set.fromList .
  map (map toLower) .
  filter (all isLetter)

mostAnagrams :: [String] -> [[String]]
mostAnagrams =
  reverse .
  map (map fst) .
  sortBy (comparing length) .
  groupBy ((==) `on` snd) .
  sortBy (comparing snd) .
  map (id &&& sort) .
  Set.toList . Set.fromList .
  map (map toLower) .
  filter (all isLetter)

main :: IO ()
main = do
  words <- lines <$> readFile "/etc/dictionaries-common/words"
  putStrLn "10 longest palindromes: "
  mapM_ print $ take 10 $ longestAnagrams words

  putStrLn "\n10 sets of words with the most anagrams:"
  mapM_ print $ take 10 $ mostAnagrams words
