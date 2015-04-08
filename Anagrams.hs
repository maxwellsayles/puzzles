{- This one speaks for itself -}
import Control.Arrow
import Control.Monad
import Data.Char
import Data.Function
import Data.List
import qualified Data.Set as Set

main = mapM_ print .
       take 10 . 
       reverse .
       sortBy (compare `on` (length . head)) .
       filter ((>1) . length) .
       map (map fst) .
       groupBy ((==) `on` snd) .
       sortBy (compare `on` snd) .
       map (id &&& sort) .
       Set.toList .
       Set.fromList .
       map (map toLower) .
       filter (all isLetter) .
       lines =<<
       readFile "/etc/dictionaries-common/words"
