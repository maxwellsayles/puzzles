"""
Sudoku solver using back-tracing and exhaustive search.
"""

import unittest

def print_sudoku(cells):
	def print_row(cells, j):
		s = ''.join(
			[
				str(x) if x is not None else ' '
				for x in [cells.get((i, j)) for i in range(9)]
			]
		)
		print("{}|{}|{}".format(s[0:3], s[3:6], s[6:9]))
	print_row(cells, 0)
	print_row(cells, 1)
	print_row(cells, 2)
	print("---+---+---")
	print_row(cells, 3)
	print_row(cells, 4)
	print_row(cells, 5)
	print("---+---+---")
	print_row(cells, 6)
	print_row(cells, 7)
	print_row(cells, 8)

def string_to_cells(str):
	cells = dict()
	for i in range(9):
		for j in range(9):
			c = str[j][i]
			if c >= '1' and c <= '9':
				cells[(i, j)] = int(c)
	return cells

def hvals(cells, j):
	return { cells.get((i, j)) for i in range(9) }.difference({None})

def vvals(cells, i):
	return { cells.get((i, j)) for j in range(9) }.difference({None})

def svals(cells, i, j):
	i = 3 * (i // 3)
	j = 3 * (j // 3)
	return { cells.get((x, y))
			 for x in range(i, i + 3)
			 for y in range(j, j + 3)
			}.difference({None})

def candidates(cells, i, j):
	vals = hvals(cells, j).union(vvals(cells, i)).union(svals(cells, i, j))
	return set(range(1, 10)).difference(vals)

def step(cells, i, j):
	if cells.get((i, j)) is not None:
		if i == 8 and j == 8:
			return cells
		if i == 8:
			return step(cells, 0, j + 1)
		return step(cells, i + 1, j)

	xs = candidates(cells, i, j)
	for x in xs:
		cs = cells.copy()
		cs[(i, j)] = x
		cs = step(cs, i, j)
		if cs is not None:
			return cs
	return None

def solve(cells):
	return step(cells, 0, 0)

class TestSudoku(unittest.TestCase):
	def test_eg1(self):
		input = string_to_cells(
			[
				"53  7    ",
				"6  195   ",
				" 98    6 ",
				"8   6   3",
				"4  8 3  1",
				"7   2   6",
				" 6    28 ",
				"   419  5",
				"    8  79",
			]
		)
		actual = solve(input)
		expected = string_to_cells(
			[
				"534678912",
				"672195348",
				"198342567",
				"859761423",
				"426853791",
				"713924856",
				"961537284",
				"287419635",
				"345286179",
			]
		)
		self.assertEqual(actual, expected)

	def test_eg2(self):
		# https://sandiway.arizona.edu/sudoku/examples.html
		# Arizona Daily Wildcat: Tuesday, Jan 17th 2006
		input = string_to_cells(
			[
				"   26 7 1",
				"68  7  9 ",
				"19   45  ",
				"82 1   4 ",
				"  46 29  ",
				" 5   3 28",
				"  93   74",
				" 4  5  36",
				"7 3 18   ",
			]
		)
		actual = solve(input)
		expected = string_to_cells(
			[
				"435269781",
				"682571493",
				"197834562",
				"826195347",
				"374682915",
				"951743628",
				"519326874",
				"248957136",
				"763418259",
			]
		)
		self.assertEqual(actual, expected)

	def test_eg3(self):
		# https://sandiway.arizona.edu/sudoku/examples.html
		# Challenge 1 from Sudoku Solver by Logic
		input = string_to_cells(
			[
				" 2       ",
				"   6    3",
				" 74 8    ",
				"     3  2",
				" 8  4  1 ",
				"6  5     ",
				"    1 78 ",
				"5    9   ",
				"       4 ",
			]
		)
		actual = solve(input)
		expected = string_to_cells(
			[
				"126437958",
				"895621473",
				"374985126",
				"457193862",
				"983246517",
				"612578394",
				"269314785",
				"548769231",
				"731852649",
			]
		)
		self.assertEqual(actual, expected)

if __name__ == '__main__':
	unittest.main()
