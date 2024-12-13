"""
Problem:
https://leetcode.com/problems/the-skyline-problem/description/
"""

import queue
import unittest

from typing import List

class Extent:
	def __init__(self, l, r, h):
		self.l = l
		self.r = r
		self.h = h

	def __lt__(self, other):
		if self.l == other.l:
			if self.h == other.h:
				return self.r < other.r
			return self.h > other.h
		return self.l < other.l

	def __repr__(self):
		return "Extent({},{},{})".format(self.l, self.r, self.h)

class Solution:
	def getSkyline(self, buildings: List[tuple[int, int, int]]) -> List[tuple[int, int]]:
		if len(buildings) == 0:
			return []

		points = set()
		for b in buildings:
			points.add(b[0])
			points.add(b[1])
		points = sorted(points)

		extents = sorted(map(lambda b: Extent(b[0], b[1], b[2]), buildings))

		q = queue.PriorityQueue()
		q.put((0, Extent(min(points), max(points), 0)))

		res = []
		i = 0
		for p in points:
			while i < len(extents) and extents[i].l <= p:
				q.put((-extents[i].h, extents[i]))
				i += 1
			while not q.empty():
				e = q.get()[1]
				if e.l <= p and e.r > p:
					if len(res) == 0 or res[-1][1] != e.h:
						res.append((p, e.h))
					q.put((-e.h, e))
					break
		res.append((max(points), 0))
		return res

class TestSkyline(unittest.TestCase):

	def test1(self):
		buildings = [[2,9,10],[3,7,15],[5,12,12],[15,20,10],[19,24,8]]
		expectedOutput = [(2,10),(3,15),(7,12),(12,0),(15,10),(20,8),(24,0)]
		actualOutput = Solution().getSkyline(buildings)
		self.assertEqual(actualOutput, expectedOutput)

	def test2(self):
		buildings = [[0,2,3],[2,5,3]]
		expectedOutput = [(0,3),(5,0)]
		actualOutput = Solution().getSkyline(buildings)
		self.assertEqual(actualOutput, expectedOutput)

	def testEmpty(self):
		self.assertEqual(Solution().getSkyline([]), [])

	def testSingle(self):
		self.assertEqual(Solution().getSkyline([(1,2,3)]), [(1,3),(2,0)])

	def testContained(self):
		buildings = [(1, 10, 5), (3, 6, 3)]
		output = [(1, 5), (10, 0)]
		self.assertEqual(Solution().getSkyline(buildings), output)

if __name__ == '__main__':
	unittest.main()
