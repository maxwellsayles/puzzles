{--
Given an array of integers, find 3 non-overlapping segments each of k elements
where the sum of the elements covered by the segments is maximal.

The strategy is this. First, from left to right, walk one segment of k elements.
At each index, store the largest sum that can be made with a segment ending at
that index. Do the same from right to left. Then for each index, place the
middle segment and in constant time, look up the largest sum made by a segment
covering it the section to the left and to the right. Since each stage takes
O(n) time and requires O(n) memory, the solution is O(n) time and memory.
-}

main :: IO ()
main = do
  print "Hello"
