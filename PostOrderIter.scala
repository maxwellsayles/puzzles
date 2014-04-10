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

// Case objects to indicate the stage of node visitation.
// This could also be done with an object that extends Enumeration.
sealed trait Visit
case object LeftVisit extends Visit
case object RightVisit extends Visit
case object FinishedVisit extends Visit

class PostOrderIter[T](t: Tree[T]) {
  var stack: List[(Tree[T], Visit)] = List((t, LeftVisit))

  def next(): Option[T] = {
    // stack is empty, we are finished.
    if (stack.isEmpty) return None

    // otherwise, inspect the top element.
    val (topNode, topVisit) :: rest = stack
    topNode match {
      // If it's a leaf, continue with the next element.
      case Leaf =>
        stack = rest
        next()

      // Either traverse the Left child, the Right child,
      // or return the current
      case Node(l, v, r) =>
        topVisit match {
          case LeftVisit =>
            stack = (l, LeftVisit) :: (topNode, RightVisit) :: rest
            next()
          case RightVisit =>
            stack = (r, LeftVisit) :: (topNode, FinishedVisit) :: rest
            next()
          case FinishedVisit =>
            stack = rest
            Some(v)
        }
    }
  }
}

def iterate[T](
  f: T => Unit,
  iter: PostOrderIter[T]
) {
  iter.next() match {
    case None =>
      return

    case Some(v) =>
      f(v)
      iterate(f, iter)
  }
}

/**
 * This is a standard post order iteration of a tree using continuations.
 * NOTE: This is my preferred solution to this problem.
 */
def iterate2[T](
  f: T => Unit,
  n: Tree[T]
) {
  def step(
    n: Tree[T],
    k: => Unit
  ) {
    n match {
      case Leaf => k
      case Node(l, v, r) =>
        step(l, {
          step(r, {
            f(v)
            k
          })
        })
    }
  }
  step(n, {})
}

def doTree(t: Tree[Int]) {
  val iter = new PostOrderIter(t)
  iterate(print, iter)
  iterate2(print, t)
  println()
}

doTree(Leaf)
doTree(Node(1))
doTree(Node(Node(1), 2, Leaf))
doTree(Node(Leaf, 1, Node(2)))
doTree(Node(Node(1), 2, Node(3)))
doTree(Node(Node(Node(1), 2, Node(3)), 5, Node(Node(6), 7, Node(8))))

