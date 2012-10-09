{-
Given two binary space partitions, where a partition can be labelled 0 or 1,
compute the binary AND of the two BSPs.
-}

data Bsp = Node Bsp Bsp
         | Zero
         | One

instance Show Bsp where
    show Zero = "0"
    show One = "1"
    show (Node l r) = "(" ++ show l ++ ", " ++ show r ++ ")"

-- When the left child and right child are the same, they can be simplied.
simplify :: Bsp -> Bsp
simplify One = One
simplify Zero = Zero
simplify (Node l r) =
    case (simplify l, simplify r) of
      (One, One) -> One
      (Zero, Zero) -> Zero
      (l, r) -> Node l r

-- Compute the BSP AND and then simplify.
bspAnd :: Bsp -> Bsp -> Bsp
bspAnd l r = simplify $ bspAnd' l r

-- A left/right child of 0 implies 0.
-- A left/right child of 1 implies the other.
-- Otherwise we recurse on the left subtree for the left child,
-- and the right subtree for the right child.
bspAnd' :: Bsp -> Bsp -> Bsp
bspAnd' Zero _ = Zero
bspAnd' _ Zero = Zero
bspAnd' One r = r
bspAnd' l One = l
bspAnd' (Node ll lr) (Node rl rr) = 
    Node (bspAnd' ll rl) (bspAnd' lr rr)

-- Run a bunch of tests.
main = do
  let a = Node (Node Zero One) One
  let b = Node One (Node Zero One)
  putStrLn $ "a = " ++ show a
  putStrLn $ "b = " ++ show b
  putStrLn $ "a&b = " ++ show (bspAnd a b)
  putStrLn ""
  
  let a = Node (Node Zero One) One
  let b = Node Zero (Node Zero One)
  putStrLn $ "a = " ++ show a
  putStrLn $ "b = " ++ show b
  putStrLn $ "a&b = " ++ show (bspAnd a b)
  putStrLn ""

  let a = Node (Node Zero One) Zero
  let b = Node Zero (Node Zero One)
  putStrLn $ "a = " ++ show a
  putStrLn $ "b = " ++ show b
  putStrLn $ "a&b = " ++ show (bspAnd a b)
  putStrLn ""

  let a = Node (Node One One) One
  let b = Node One (Node One One)
  putStrLn $ "a = " ++ show a
  putStrLn $ "b = " ++ show b
  putStrLn $ "a&b = " ++ show (bspAnd a b)
  putStrLn ""
       