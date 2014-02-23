data Kilometers = Kilometers
data Miles = Miles

newtype Distance a b = Distance a deriving Show

kilometers :: a -> Distance a Kilometers
kilometers x = Distance x

miles :: a -> Distance a Miles
miles x = Distance x

add :: Num a => Distance a b -> Distance a b -> Distance a b
add (Distance x) (Distance y) = Distance $ x + y

main = do
  let x = kilometers 3
  let y = kilometers 5
  let z = miles 7
  print $ add x y
--  print $ add x z  -- compile time error