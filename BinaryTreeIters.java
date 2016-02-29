import java.util.Iterator;
import java.util.Stack;

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

class PreOrderIter<E> implements Iterator<E> {
    public PreOrderIter(Tree<E> n) {
	stack.push(n);
    }

    public E next() {
	Tree<E> n = stack.pop();
	if (n.r != null) {
	    stack.push(n.r);
	}
	if (n.l != null) {
	    stack.push(n.l);
	}
	return n.v;
    }

    public boolean hasNext() {
	return !stack.empty();
    }

    public void remove() {
	throw new UnsupportedOperationException();
    }

    private Stack<Tree<E>> stack = new Stack<>();
}

class InOrderIter<E> implements Iterator<E> {
    public InOrderIter(Tree<E> n) {
	findLeft(n);
    }

    public E next() {
	Tree<E> n = path.pop();
	findLeft(n.r);
	return n.v;
    }

    public boolean hasNext() {
	return !path.empty();
    }

    public void remove() {
	throw new UnsupportedOperationException();
    }

    private void findLeft(Tree<E> n) {
	while (n != null) {
	    path.push(n);
	    n = n.l;
	}
    }

    private Stack<Tree<E>> path = new Stack<>();
}

class PostOrderIter<E> implements Iterator<E> {
    public PostOrderIter(Tree<E> n) {
	findLeaf(n);
    }

    public E next() {
	Tree<E> n = path.pop();
	if (!path.empty()) {
	    Tree<E> p = path.peek();
	    if (p.l == n) {
		findLeaf(p.r);
	    }
	}
	return n.v;
    }

    public boolean hasNext() {
	return !path.empty();
    }

    public void remove() {
	throw new UnsupportedOperationException();
    }

    private void findLeaf(Tree<E> n) {
	while (n != null) {
	    path.push(n);
	    if (n.l != null) {
		n = n.l;
	    } else {
		n = n.r;
	    }
	}
    }

    private Stack<Tree<E>> path = new Stack<>();
}

public class BinaryTreeIters {
    private static Tree<Integer> tree(int v) {
	return new Tree<>(v);
    }

    private static Tree<Integer> tree(Tree<Integer> l, int v, Tree<Integer> r) {
	return new Tree<>(l, v, r);
    }

    public static void main(String[] args) {
	Tree<Integer> t = tree(tree(tree(1), 2, tree(3)),
			       4,
			       tree(tree(5), 6, tree(7)));

	Iterator<Integer> iter = new PreOrderIter<>(t);
	while (iter.hasNext()) {
	    System.out.print(iter.next() + " ");
	}
	System.out.println();

	iter = new InOrderIter<>(t);
	while (iter.hasNext()) {
	    System.out.print(iter.next() + " ");
	}
	System.out.println();

	iter = new PostOrderIter<>(t);
	while (iter.hasNext()) {
	    System.out.print(iter.next() + " ");
	}
	System.out.println();
    }
}
