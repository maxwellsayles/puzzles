"""
The naive method iterates each haystack subset of needle length and compares
the sorted subset with the sorted needle. This takes O(n*m*logm) time and O(n)
memory.

The linear method first computes the character frequency of the needle.
Then iterates two indices, the length of the needle + 1 apart. The smaller
index is one character before the subset considered. Then or the smaller index,
the frequency for that character is incremented, and for the larger index, the
frequency for that character is decremented. For any frequency that is 0, the
entry is deleted. When the frequency dict is empty, the value of the smaller
index + 1 is added to the result. This takes O(n) time and O(n) memory.
"""

import unittest

from collections import Counter

def anagram_indices_naive(needle, haystack):
	res = []
	n = len(needle)
	needle_sorted = sorted(needle)
	for i in range(len(haystack) - n + 1):
		sub = sorted(haystack[i : i + n])
		if sub == needle_sorted:
			res.append(i)
	return res

def anagram_indices_linear(needle, haystack):
	n = len(needle)
	freq = Counter(needle)
	res = []
	for i in range(len(haystack)):
		if i - n >= 0:
			c = haystack[i - n]
			freq[c] = (freq.get(c) or 0) + 1
			if freq[c] == 0:
				del freq[c]

		c = haystack[i]
		freq[c] = (freq.get(c) or 0) - 1
		if freq[c] == 0:
			del freq[c]

		if len(freq) == 0:
			res.append(i - n + 1)

	return res

class TestAnagramIndices(unittest.TestCase):

	def test1(self):
		expected = [0, 3, 4]

		actual = anagram_indices_naive('ab', 'abxaba')
		self.assertEqual(actual, expected)

		actual = anagram_indices_linear('ab', 'abxaba')
		self.assertEqual(actual, expected)



if __name__ == '__main__':
	unittest.main()

