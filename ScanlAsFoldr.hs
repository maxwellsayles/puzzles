> import Prelude hiding (foldr, scanl)

This is one possible implementation of 'scanl'.

> scanl f x0 [] = [x0]
> scanl f x0 (x:xs) = x0 : scanl f (f x0 x) xs

Notice that if scanl swapped its last two arguments so that x0
came at the end, it could be rewritten as

> scanl' f []     = \x0 -> [x0]
> scanl' f (x:xs) = \x0 -> x0 : (scanl' f xs) (f x0 x)

The parenthesis around (scanl' f xs) is for emphasis.
  
scanl_cont uses a foldr (with eta-expansion) to compute the
same thing, in the same way.  An implementation of foldr could
look like

> foldr g z0 [] = z0
> foldr g z0 (z:zs) = g z (foldr g z0 zs)

We define scanl_cont as

> scanl_cont f x0 xs = (foldr combine term xs) x0
>     where term x0 = [x0]
>           combine x k x0 = x0 : k (f x0 x)

Again, the parenthesis around (foldr combine term xs) is for
emphasis. When we expand using the definition of foldr above, we get

foldr combine term (x:xs) = \x0 -> x0 : (foldr combine term xs) (f x0 x)

As such, x0 is the head of list, and when (foldr combine term xs)
is expanded again, (f x0 x) is the next head of list and we
end up with the same form above.  This continues until the end
of the list, at which point foldr expands to

foldr combine term [] = \x0 -> [x0]

Since, we both compute the list starting at the head, and consume
the list from the head, this works on infinite lists.

Notice, we defined scanl' like
    
scanl' f []     = \x0 -> [x0]
scanl' f (x:xs) = \x0 -> x0 : (scanl' f xs) (f x0 x)

and scanl_cont expands to

foldr combine term []     = \x0 -> [x0]
foldr combine term (x:xs) = \x0 -> x0 : (foldr combine term xs) (f x0 x)

In the second form, (foldr combine term xs) takes the place of
(scanl' f xs).

> main = do
>   print $ take 20 $ scanl (+) 0 [1..]
>   print $ take 20 $ scanl_cont (+) 0 [1..]