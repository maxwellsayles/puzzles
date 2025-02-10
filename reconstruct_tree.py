"""
Reconstruct a binary tree given the pre-order and in-order traversal of the
tree values.
"""

import unittest

class Node:
	def __init__(self, value, left, right):
		self.value = value
		self.left = left
		self.right = right

def in_order(node):
	if not node:
		return
	yield from in_order(node.left)
	yield node.value
	yield from in_order(node.right)

def pre_order(node):
	if not node:
		return
	yield node.value
	yield from pre_order(node.left)
	yield from pre_order(node.right)

def post_order(node):
	if not node:
		return
	yield from post_order(node.left)
	yield from post_order(node.right)
	yield node.value

def pre_in_order_tree(pre_order, in_order):
	if not pre_order:
		return None
	root = pre_order[0]
	idx = in_order.index(root)
	left = pre_in_order_tree(pre_order[1:idx+1], in_order[:idx])
	right = pre_in_order_tree(pre_order[idx+1:], in_order[idx+1:])
	return Node(root, left, right)

def post_in_order_tree(post_order, in_order):
	if not post_order:
		return None
	root = post_order[-1]
	idx = in_order.index(root)
	left = post_in_order_tree(post_order[:idx], in_order[:idx])
	right = post_in_order_tree(post_order[idx:-1], in_order[idx+1:])
	return Node(root, left, right)

class TestPreInOrderTree(unittest.TestCase):

	def test_empty(self):
		self.assertEqual(pre_in_order_tree([], []), None)

	def test_single(self):
		node = pre_in_order_tree('a', 'a')
		self.assertEqual(node.value, 'a')
		self.assertEqual(node.left, None)
		self.assertEqual(node.right, None)

	def test_eg1_pre_in_order(self):
		node = pre_in_order_tree('abdecfg', 'dbeafcg')
		self.assertEqual(''.join(pre_order(node)), 'abdecfg')
		self.assertEqual(''.join(in_order(node)), 'dbeafcg')

	def test_eg1_post_in_order(self):
		node = post_in_order_tree('bdecfga', 'dbeafcg')
		self.assertEqual(''.join(post_order(node)), 'bdecfga')
		self.assertEqual(''.join(in_order(node)), 'dbeafcg')

if __name__ == '__main__':
	unittest.main()
