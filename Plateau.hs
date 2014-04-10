{- |
A plateau is an sublist of 2 or more of the same elements,
where the sublist is not contained within a larger sublist
of the same elements.

plateaus [1,2,2,3,4,4,4,5,6,6,7] == [[2, 2], [4, 4, 4], [6, 6]]
-}

plateaus [] = []
plateaus (x:xs) = f xs [x]
    where f [] acc@(_:_:_) = [acc]
          f [] _ = []
          f (x:xs) acc@(y:_) | x == y = f xs (x:acc)
          f (x:xs) [_] = f xs [x]
          f (x:xs) acc = acc : f xs [x]


