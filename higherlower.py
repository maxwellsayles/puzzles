"""
Given an input string of +/-, output an array of integers such that for each
pair of consecutive integers in the output, the second integer is either higher
(for an input +) or lower (for a -), and the output contains the integers [1..N].

E.g.
Input: '++-+'
Output: either [1, 2, 4, 3, 5] or [1, 2, 5, 3, 4]
"""

import unittest

def higher_lower(xs):
	res = []
	v = 1
	c = 1
	for x in xs:
		if x == '-':
			c += 1
		else:
			for i in reversed(range(v, v + c)):
				res.append(i)
			v += c
			c = 1
	for i in reversed(range(v, v + c)):
		res.append(i)
	return res

def is_valid(xs, actual):
	'''
	Verify that:
	- actual is the length of xs + 1.
	- actual contains the integers [1..N]
	- xs corresponds to the higher/lower of actual pairs.
	- Generating +/- from actual pairs matches the input xs.
	'''
	if len(xs) + 1 != len(actual):
		return False

	if sorted(actual) != list(range(1, 1 + len(actual))):
		return False

	for i, c in enumerate(xs):
		if c == '-' and actual[i] <= actual[i + 1]:
			return False
		if c == '+' and actual[i] >= actual[i + 1]:
			return False

	hl = ''.join(
		map(
			lambda p: '+' if p[0] < p[1] else '-',
			zip(actual[:-1], actual[1:]),
		)
	)
	return xs == hl

class TestHigherLower(unittest.TestCase):

	def test_is_valid(self):
		self.assertTrue(is_valid('++-+', [1, 2, 4, 3, 5]))
		self.assertTrue(is_valid('++-+', [1, 2, 5, 3, 4]))

	def test_empty(self):
		self.assertEqual(higher_lower(''), [1])

	def test_one(self):
		self.assertEqual(higher_lower('+'), [1, 2])
		self.assertEqual(higher_lower('-'), [2, 1])

	def test_two(self):
		self.assertTrue(is_valid('++', higher_lower('++')))
		self.assertTrue(is_valid('+-', higher_lower('+-')))
		self.assertTrue(is_valid('--', higher_lower('--')))
		self.assertTrue(is_valid('-+', higher_lower('-+')))

	def test_three(self):
		self.assertTrue(is_valid('+++', higher_lower('+++')))
		self.assertTrue(is_valid('++-', higher_lower('++-')))
		self.assertTrue(is_valid('+--', higher_lower('+--')))
		self.assertTrue(is_valid('+-+', higher_lower('+-+')))
		self.assertTrue(is_valid('-++', higher_lower('-++')))
		self.assertTrue(is_valid('-+-', higher_lower('-+-')))
		self.assertTrue(is_valid('---', higher_lower('---')))
		self.assertTrue(is_valid('--+', higher_lower('--+')))

	def test_eg1(self):
		self.assertTrue(is_valid('++-+', higher_lower('++-+')))

if __name__ == '__main__':
	unittest.main()
