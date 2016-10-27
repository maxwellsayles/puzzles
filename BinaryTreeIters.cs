using System;
using System.Collections.Generic;

namespace PPG {

class Tree<E> {
    public Tree(E v) {
	this.v = v;
    }

    public Tree(Tree<E> l, E v, Tree<E> r) {
	this.l = l;
	this.v = v;
	this.r = r;
    }

    public Tree<E> l = null;
    public E v;
    public Tree<E> r = null;
}

class PreOrderIter<E> {
    public PreOrderIter(Tree<E> n) {
	stack.Push(n);
    }

    public E next() {
	var n = stack.Pop();
	if (n.r != null) {
	    stack.Push(n.r);
	}
	if (n.l != null) {
	    stack.Push(n.l);
	}
	return n.v;
    }

    public bool hasNext() {
	return stack.Count != 0;
    }

    private Stack<Tree<E>> stack = new Stack<Tree<E>>();
}

class InOrderIter<E> {
    public InOrderIter(Tree<E> n) {
	findLeft(n);
    }

    public E next() {
	var n = path.Pop();
	findLeft(n.r);
	return n.v;
    }

    public bool hasNext() {
	return path.Count != 0;
    }

    private void findLeft(Tree<E> n) {
	while (n != null) {
	    path.Push(n);
	    n = n.l;
	}
    }

    private Stack<Tree<E>> path = new Stack<Tree<E>>();
}

class PostOrderIter<E> {
    public PostOrderIter(Tree<E> n) {
	findLeaf(n);
    }

    public E next() {
	var n = path.Pop();
	if (path.Count != 0) {
	    var p = path.Peek();
	    if (p.l == n) {
		findLeaf(p.r);
	    }
	}
	return n.v;
    }

    public bool hasNext() {
	return path.Count != 0;
    }

    private void findLeaf(Tree<E> n) {
	while (n != null) {
	    path.Push(n);
	    if (n.l != null) {
		n = n.l;
	    } else {
		n = n.r;
	    }
	}
    }

    private Stack<Tree<E>> path = new Stack<Tree<E>>();
}

public class BinaryTreeIters {
    private static Tree<int> tree(int v) {
	return new Tree<int>(v);
    }

    private static Tree<int> tree(Tree<int> l, int v, Tree<int> r) {
	return new Tree<int>(l, v, r);
    }

    public static void Main(string[] args) {
	var t = tree(tree(tree(1), 2, tree(3)),
			       4,
			       tree(tree(5), 6, tree(7)));

	var pre = new PreOrderIter<int>(t);
	while (pre.hasNext()) {
	    Console.Write(pre.next() + " ");
	}
	Console.WriteLine();

	var inorder = new InOrderIter<int>(t);
	while (inorder.hasNext()) {
	    Console.Write(inorder.next() + " ");
	}
	Console.WriteLine();

	var post = new PostOrderIter<int>(t);
	while (post.hasNext()) {
	    Console.Write(post.next() + " ");
	}
	Console.WriteLine();
    }
}

}