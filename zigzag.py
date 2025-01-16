import unittest

def zigzag(s, n):
	assert(n >= 1)
	if n == 1:
		return [s]

	y = 0
	dy = 1
	res = ['' for _ in range(n)]
	for c in s:
		res = [
			r + c if i == y else r + ' '
			for i, r in enumerate(res)
		]
		if y == 0 and dy == -1:
			dy = 1
		if y == n - 1 and dy == 1:
			dy = -1
		y += dy
	return res

class TestZigZag(unittest.TestCase):

	def test0(self):
		with self.assertRaises(AssertionError):
			zigzag('thisisazigzag', 0)

	def test1(self):
		actual = zigzag('thisisazigzag', 1)
		expected = ['thisisazigzag']
		self.assertEqual(actual, expected)

	def test2(self):
		actual = zigzag('thisisazigzag', 2)
		expected = ['t i i a i z g', ' h s s z g a ']
		self.assertEqual(actual, expected)

	def test3(self):
		actual = zigzag('thisisazigzag', 3)
		expected = ['t   i   i   g', ' h s s z g a ', '  i   a   z  ']
		self.assertEqual(actual, expected)

	def test4(self):
		actual = zigzag('thisisazigzag', 4)
		expected = [
			't     a     g',
			' h   s z   a ',
			'  i i   i z  ',
			'   s     g   ',
		]
		self.assertEqual(actual, expected)

	def test13(self):
		actual = zigzag('thisisazigzag', 13)
		expected = [
			't            ',
			' h           ',
			'  i          ',
			'   s         ',
			'    i        ',
			'     s       ',
			'      a      ',
			'       z     ',
			'        i    ',
			'         g   ',
			'          z  ',
			'           a ',
			'            g',
		]
		self.assertEqual(actual, expected)

if __name__ == '__main__':
	unittest.main()
