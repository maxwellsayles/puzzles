/**
 * Implements a post order binary tree iterator using a stack
 * of lambdas.  Each lambda represents the thing to do next.
 * Each node pushes three lambdas onto the stack.  The first is
 * for returning the value of the node, the second for processing
 * the right subtree, and the third for processing the left subtree.
 * This is in reverse order from which the operations are performed
 * since a Stack is a FILO.
 * 
 * The idea here is an abstraction of the recursive approach, namely
 * 
 * OpNode(Tree<T> t)
 * {
 *   if (t == null) return;
 *   OpNode(t.l);
 *   OpNode(t.r);
 *   OpValue(t.v)
 * }
 * 
 * Too bad C# requires so much boiler plate and uses a sparse layout.
 */

using System;
using System.Collections.Generic;

namespace PostOrderIter
{
  class PostOrderIter<T>
  {
    public PostOrderIter(Tree<T> t)
    {
      PushNode(t);
    }
    
    public bool HasNext()
    {
      return ops.Count != 0;
    }
    
    public T Next()
    {
      return ops.Pop()();
    }

    delegate T Op();
    private Stack<Op> ops = new Stack<Op>();

    private void PushNode(Tree<T> t)
    {
      if (t != null)
	{
	  ops.Push(() => t.v);
	  ops.Push(() => { PushNode(t.r); return Next(); });
	  ops.Push(() => { PushNode(t.l); return Next(); });
	}
    }
  }

  class Tree<T>
  {
    public T v;
    public Tree<T> l;
    public Tree<T> r;
    public Tree(T x)
    {
      v = x;
      l = null;
      r = null;
    }
    public Tree(Tree<T> l, T v, Tree<T> r)
    {
      this.v = v;
      this.l = l;
      this.r = r;
    }
  }

  class Program 
  {
    static Tree<int> Tree(int v)
    {
      return new Tree<int>(v);
    }

    static Tree<int> Tree(Tree<int> l, int v, Tree<int> r)
    {
      return new Tree<int>(l, v, r);
    }

    static Tree<int> Tree(int l, int v, int r)
    {
      return Tree(Tree(l), v, Tree(r));
    }

    static void DoTree(Tree<int> t)
    {
      var iter = new PostOrderIter<int>(t);
      while (iter.HasNext())
      {
	Console.Write("{0} ", iter.Next());
      }
      Console.WriteLine();
    }
        

    static void Main(string[] args)
    {
      // Print nothing.
      DoTree(null);
      // Print 1.
      DoTree(Tree(1));
      // Print 1 3 2.
      DoTree(Tree(1, 2, 3));
      // Print 1 3 2 5 7 6 4.
      DoTree(Tree(Tree(1, 2, 3), 4, Tree(5, 6, 7)));
    }
  }
}
