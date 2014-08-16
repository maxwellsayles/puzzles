/**
 * Given a binary tree, provide a post order iterator.  The expected
 * solution is one that provides a stateful class and is O(n) over the tree.
 * 
 * I also provide a standard post order recursive solution that uses
 * continuations to avoid stack overflow.
 */

// Covariant tree model.
sealed trait Tree[+T]
case object Leaf extends Tree[Nothing]
case class Node[+T](left: Tree[T], value: T, right: Tree[T]) extends Tree[T]
object Node {
  def apply[T](value: T): Node[T] = Node(Leaf, value, Leaf)
}

/***
 * Stateful post order binary tree iterator using lambdas.
 */
class PostOrderIter[T](t: Tree[T]) {
  var ops: List[() => T] = List()
  pushNode(t)

  def hasNext(): Boolean = !ops.isEmpty

  // Pop an op.
  def next(): T = {
    val op = ops.head
    ops = ops.tail
    op()
  }

  private def pushNode: Tree[T] => Unit = {
    case Node(l, v, r) =>
      ops = (() => v) :: ops
      ops = (() => opNode(r)) :: ops
      ops = (() => opNode(l)) :: ops
    case Leaf => ()
  }

  private def opNode(t: Tree[T]): T = {
    pushNode(t)
    next()
  }
}

/**
 * This is a standard post order iteration of a tree using continuations.
 * NOTE: This is my preferred solution to this problem.
 */
def iterate[T](
  f: T => Unit,
  n: Tree[T],
  k: => Unit = {}
) {
  n match {
    case Leaf => k
    case Node(l, v, r) =>
      iterate(f, l, {
        iterate(f, r, {
          f(v)
          k
        })
      })
  }
}

def doTree(t: Tree[Int]) {
  val iter = new PostOrderIter(t)
  while (iter.hasNext()) {
    print(iter.next())
  }
  print(' ')
  iterate(print, t)
  println()
}

doTree(Leaf)
doTree(Node(1))
doTree(Node(Node(1), 2, Leaf))
doTree(Node(Leaf, 1, Node(2)))
doTree(Node(Node(1), 2, Node(3)))
doTree(Node(Node(Node(1), 2, Node(3)), 5, Node(Node(6), 7, Node(8))))

