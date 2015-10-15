"""
A loose reversal of the string X = x_1x_2...x_n is to break it up into m
contiguous substrings, reversing some substrings while not reversing other
substrings, then concatenating the substrings in the original order to form
Y = y_1y_2...y_n.  The cost of the loose reversal is the number of m
contiguous substrings.

This program compares a dynamic programming solution that is optimal
with a greedy algorithm that is close to optimal, but not optimal
on all input.

Written by Maxwell Sayles, 2008.
"""
import sys

def looseReversal(A, B):
	"""
	The dynamic programming recurrence is:
	C[0] = 0
	C[1] = 1 + \min_{1 <= j <= i} 
	    \begin{cases}
			C[j-1] if x_j...x_i = y_j...y_i
			C[j-1] if (x_j...x_i)^R = y_j...y_i
			infinity otherwise
		\end{cases}
	return C[n]

	Since C[i] depends on C[j] where j<i, we populate C from 0 to n.

	This algorithm runs in O(n^3) but can be made to run in O(n^2) by
	precomputing the reversal of A.
	"""
	if len(A) != len(B):
		return sys.maxint

	n = len(A)
	C = [0] * (n+1) # construct the array of n+1 elements

	# populate the array
	for i in xrange(1,n+1): # loops O(n) times
		m = sys.maxint
		
		# find the minimum j
		for j in xrange(1,i+1): # loops O(n) times
			X = A[j-1:i] # -1 for zero base indexing
			Y = B[j-1:i]
			if X == Y:
				if 1+C[j-1] < m:
					m = 1+C[j-1]
			elif X[::-1] == Y:  # string reversal syntax, takes O(n)
				if 1+C[j-1] < m:
					m = 1+C[j-1]
		C[i] = m
	return C[n]

def looseReversal_greedy(A, B):
	"""
	This greedy variant is to start at the left side and work right
	matching characters as we go.  Once a character does not match, start
	at the right side and find the largest substring that reverses to match.

	The greedy variant fails to give the optimal solution on all substrings
	for example, 'abca' and 'acba' return 3 because the first 'a' matches, then
	'bc' is matched with 'cb' and then the final 'a' is matched.  An optimal
	solution is to reverse the entire string.

	The greedy solution can be made to run in linear time O(n) by precomputing
	the string reversal, however, the greedy solution is not optimal on all
	input.

	A second greey variant not shown here is to start at the left, matching
	characters.  Once a character does not match, continue scanning right
	until we find the first reversal that does not match.  This variant
	does worse than the above since the above will give an optimal solution
	for 'abcabc' and 'cbacba' but this one fails to do so.
	"""
	if len(A) != len(B):
		return sys.maxint

	n = len(A)
	c = 0 # number of substrings
	i = 0
	while i < n:
		if A[i] == B[i]:
			# characters match, nothing to do
			i += 1
			if i == n or A[i] != B[i]:
				c += 1
		else:
			# characters don't match
			# find the largest substring starting at i
			# that matches when reversed
			j = n-1
			looping = True
			while looping:
				X = A[i:j+1]
				Y = B[i:j+1]
				if X[::-1] == Y:
					# there is a matching reverse
					c += 1 # increment the substring count
					i = j+1	 # continue at the next character
					looping = False
				j -= 1
				if j == i:
					# no loose reversal is possible
					return sys.maxint
	return c

if __name__ == '__main__':
	A = 'ilovesaladnuts'
	B = 'olivesandaltus'
	print 'D %s, %s: %d' % (A, B, looseReversal(A, B))
	print 'G %s, %s: %d' % (A, B, looseReversal_greedy(A, B))

	A = 'astrtbrabrb'
	B = 'atrtsrbrbab'
	print 'D %s, %s: %d' % (A, B, looseReversal(A, B))
	print 'G %s, %s: %d' % (A, B, looseReversal_greedy(A, B))

	A = 'abca'
	B = 'acba'
	print 'D %s, %s: %d' % (A, B, looseReversal(A, B))
	print 'G %s, %s: %d' % (A, B, looseReversal_greedy(A, B))

	A = 'dabca'
	B = 'dacba'
	print 'D %s, %s: %d' % (A, B, looseReversal(A, B))
	print 'G %s, %s: %d' % (A, B, looseReversal_greedy(A, B))

	A = 'abcad'
	B = 'acbad'
	print 'D %s, %s: %d' % (A, B, looseReversal(A, B))
	print 'G %s, %s: %d' % (A, B, looseReversal_greedy(A, B))

	A = 'abcabc'
	B = 'cbacba'
	print 'D %s, %s: %d' % (A, B, looseReversal(A, B))
	print 'G %s, %s: %d' % (A, B, looseReversal_greedy(A, B))
	
	A = 'abcdeabc'
	B = 'cbadecba'
	print 'D %s, %s: %d' % (A, B, looseReversal(A, B))
	print 'G %s, %s: %d' % (A, B, looseReversal_greedy(A, B))

	# should print an extremely large number representing infinity
	A = 'abcdfabc'
	B = 'cbadecba'
	print 'D %s, %s: %d' % (A, B, looseReversal(A, B))
	print 'G %s, %s: %d' % (A, B, looseReversal_greedy(A, B))

	A = 'thesamestringforAandB'
	B = 'thesamestringforAandB'
	print 'D %s, %s: %d' % (A, B, looseReversal(A, B))
	print 'G %s, %s: %d' % (A, B, looseReversal_greedy(A, B))

	# a particular greedy approach (not given here) will fail on this input
	A = 'abcbaa'
	B = 'abcbaa'
	print 'D %s, %s: %d' % (A, B, looseReversal(A, B))
	print 'G %s, %s: %d' % (A, B, looseReversal_greedy(A, B))

	A = 'abcdefcba'
	B = 'abcfedcba'
	print 'D %s, %s: %d' % (A, B, looseReversal(A, B))
	print 'G %s, %s: %d' % (A, B, looseReversal_greedy(A, B))
