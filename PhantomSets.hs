{-|
This implements PhantomSets.

The idea is that the PhantomSet defined below has no members
that point to set data, only members that point to functions
that operate on the set.  The set data is referenced through
closures that the functions operate on.
-}
import qualified Data.Set as Set

data PhantomSet a = PhantomSet { contains :: a -> Bool,
                                 insert   :: a -> PhantomSet a }

empty :: Ord a => PhantomSet a
empty = helper Set.empty
    where helper set = let contains x = x `Set.member` set
                           insert x   = helper $! x `Set.insert` set
                       in  PhantomSet contains insert

main = do
  let s = foldl insert empty [1..10]
  print $ s `contains` 5
  print $ s `contains` 11