{- | Experiments with CodensityT
Using an intentially right associative monad, we show that computing with
monadic composition or with the CodensityT monad can reduce the runtime.

One way to visualize the operations in this program is to consider the series
of operations
  1 + 1 + 1 + 1 + 1 + ... + 1 + 1
but where each addition takes time proportional to the left side input.

If we left associate we get
  ((((((1 + 1) + 1) + 1) + 1) + ...) + 1) + 1
and the last addition costs (n - 1).  The sum of all additions is O(n^2).

If we right associate we get
  1 + (1 + (1 + (1 + (1 + (... + (1 + 1))))))
and each addition costs 1 operation for a total O(n) operations.

The first solution below uses a left fold applying bind (>>=) to 10,000 monadic
functions.  The second solution uses a right fold applying Kleisli composition
to the same 10,000 monadic functions, and is significantly faster.  The third and
fourth solutions are left and right folds using the CodensityT monad, and have
about the same performance metrics as the second solution.
-}

{-# LANGUAGE Rank2Types #-}

import Control.Monad
import Control.Monad.Trans
import System.CPUTime

newtype CodensityT m a = CodensityT { runCodensityT :: forall b. (a -> m b) -> m b }

instance Monad (CodensityT f) where
  return x = CodensityT (\k -> k x)
  m >>= k = CodensityT (\c -> runCodensityT m (\a -> runCodensityT (k a) c))

instance MonadTrans CodensityT where
  lift m = CodensityT (m >>=)

lowerCodensityT :: Monad m => CodensityT m a -> m a
lowerCodensityT a = runCodensityT a return

{- |
`Custom` is an intentially right associative monad.  It was specifically
engineered so that the standard bind (>>=) operation will take O(n^2) for
n binds.  This has similar behaviour to (++) in that a left fold takes O(n^2),
while a right fold takes O(n).
-}
newtype Custom a = Custom { runCustom :: [a] }

instance Monad Custom where
  return a = Custom [a]
  Custom (x:xs) >>= f =
    let Custom (y:ys) = f x
    in  Custom $ map (const y) (x:xs) ++ (y:ys)

instance Show (Custom a) where
  show (Custom xs) = show $ length xs

step :: (Monad m, Enum a) => a -> m a
step = return . succ

-- Left fold using bind.
solution1 =
  foldl (>>=) (return 1 :: Custom Int) $
  replicate 10000 step

-- Right fold using Kleisli composition.
solution2 =
  let f' = foldr (>=>) (return :: Int -> Custom Int) $
           replicate 10000 step
  in  f' 1

-- Left fold using CodensityT.
solution3 =
  lowerCodensityT $
  foldl (>>=) (return 1 :: CodensityT Custom Int) $
  replicate 10000 (lift . step)

-- Right fold uisng CodensityT and Kleisli composition.
solution4 =
  let f' = foldr (>=>) (return :: Int -> CodensityT Custom Int) $
           replicate 10000 (lift . step)
  in  lowerCodensityT $ f' 1

timeit x = do
  time1 <- getCPUTime
  print x
  time2 <- getCPUTime
  print $ time2 - time1  

main = do
  timeit solution1
  timeit solution2
  timeit solution3
  timeit solution4
