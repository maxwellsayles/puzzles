data Tree a = Empty | Node (Tree a) a (Tree a)
            deriving Show

newtype Cont a = Cont (Maybe (a, Cont a))

post :: Tree a -> Cont a -> Cont a
post Empty k = k
post (Node l v r) k = post l $ post r $ Cont (Just (v, k))

singleton x = Node Empty x Empty

printIt t = printIt' $ post t (Cont Nothing)
printIt' (Cont Nothing) = return ()
printIt' (Cont (Just (v, k))) = do
  print v
  printIt' k

main = do
  let t = Node (singleton 1) 2 (singleton 3)
  printIt t
