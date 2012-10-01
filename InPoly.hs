{-
Given the points to a convex polygon in clock-wise order and a
point, determine if the point is within the polygon.

Here we present an O(logn) algorithm.

When there are less than 3 points, this does not form a polygon
so it is false. 

Otherwise, we take the first, last, and middle points in the list.
This forms a triangle.  If the test point is in this triangle, we
are done.  If the test point is outside of the triangle, we test
whether it is outside between the first and middle point or outside
between the middle and last point, and then we recurse appropriately.
Each time, the list of points is halved and this gives the O(logn)
runtime.
-}

import Data.Array

(x1, y1) `dot` (x2, y2) = x1 * x2 + y1 * y2
(x2, y2) `diff` (x1, y1) = (x2 - x1, y2 - y1)
inv (x, y) = (y, -x)

inTriangle p1 p2 p3 px =
    (inv v12 `dot` v1p, inv v23 `dot` v2p, inv v31 `dot` v3p)
    where v12 = p2 `diff` p1
          v23 = p3 `diff` p2
          v31 = p1 `diff` p3
          v1p = px `diff` p1
          v2p = px `diff` p2
          v3p = px `diff` p3

inPoly ps p = inPoly' ps' p 1 n
    where n = length ps
          ps' = listArray (1, n) ps

inPoly' ps p i j
    | n < 3 = False
    | d12 > 0 = inPoly' ps p i m
    | d23 > 0 = inPoly' ps p m j
    | d31 > 0 = False
    | otherwise = True
    where n = j + 1 - i
          m = (n `div` 2) + i
          (d12, d23, d31) = inTriangle (ps!i) (ps!m) (ps!j) p
