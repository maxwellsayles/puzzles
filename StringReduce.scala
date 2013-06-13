/**
  * Given a string of 'a', 'b', and 'c' only,
  * two different adjacent characters can be replaced by the third.
  * What is the length of the shortest possible string that can be made?
  * 
  * There is a trick listed on career cup.  You count the number of each
  * character.  If two or more occur 0 times, then the string cannot be
  * reduced.  If all counts are even or all counts are odd, then the
  * smallest possible is 2, otherwise, it is 1.
  * 
  * Here we also use a greedy algorithm that should run in O(n), which
  * was originally suggested by Chris Thornton.
  *
  * We first convert the string to a custom doubly linked list (we use
  * two lists one representing forward and one representing backward).
  * We then iterate the string from left to right, and any time two
  * characters can be reduced without making a string of all one
  * character that is longer than 2, we do so.  We always look one place
  * to the left of the current location when reducing.  If this fails,
  * we check the current location, and if this fails, then we continue
  * on the next location.
  */

// An immutable doubly linked list of chars, that also maintains a
// histogram. Only the characters 'a', 'b', and 'c' are considered valid.
// A string is valid when it contains at least 2 different characters
// or when it only has one type of character and is no longer than 2
// characters.
class String3(hist: Map[Char, Int], front: List[Char], back: List[Char]) {
  def this(s: String) = this(String3.histFromString(s), s.toList, Nil)

  def hasPrev: Boolean = !back.isEmpty

  def hasNext: Boolean = !front.isEmpty

  def prev: String3 = new String3(hist, back.head :: front, back.tail)

  def next: String3 = new String3(hist, front.tail, front.head :: back)

  def elem: Char = front.head

  // Insert x before the current character.
  def insert(x: Char): String3 = {
    val hist1 = hist + (x -> (hist(x) + 1))
    new String3(hist1, x :: front, back)
  }

  def remove: String3 = {
    val x = front.head
    val hist1 = hist + (x -> (hist(x) - 1))
    new String3(hist1, front.tail, back)
  }

  override def toString: String = back.reverse.mkString + front.mkString

  def isReduced: Boolean = hist.values.count(_ == 0) >= 2

  def isValid: Boolean = {
    val zeros = hist.values.count(_ == 0)
    val sum = hist.values.sum
    (zeros < 2) || (zeros >= 2 && sum <= 2)
  }
}

// Companion object that computes the histogram from an input string.
object String3 {
  def histFromString(s: String) =
    "abc".foldLeft(Map[Char, Int]()) { (acc, x) =>
      acc + (x -> s.count(_ == x)) }
}

object StringReduce {
  // This is the simple math approach.
  def mathy(s: String): Int = {
    val hist = for (c <- "abc") yield s.count(_ == c)
    if (hist.exists(_ == s.length)) s.length
    else {
      hist.map(_ % 2).sum match {
        case 0 => 2
        case 3 => 2
        case _ => 1
      }
    }
  }

  // And this is the greedy approach.  It does provide a reduced string,
  // but we are not interested in that.
  def greedy(s: String): Int = {
    // Try to treduce the two characters at the current location.
    def tryReduce(s: String3): Option[String3] = {
      if (s.hasNext && s.next.hasNext) {
        val (x, y) = (s.elem, s.next.elem)
        if (x == y) None
        else {
          val z = ('a' + 'b' + 'c' - x.toInt - y.toInt).toChar
          val s1 = s.remove.remove.insert(z)
          if (s1.isValid) Some(s1) else None
        }
      } else None
    }

    // Repeatedly try to reduce the previous position,
    // followed by the current position, followed by the next position.
    def reduce: String3 => String3 = {
      case s if s.isReduced => s
      case s =>
        val prev = if (s.hasPrev) tryReduce(s.prev) else None
        reduce((prev orElse tryReduce(s)) getOrElse s.next)
    }

    val s1 = new String3(s)
    if (s1.isReduced) s.length
    else reduce(s1).toString.length
  }

  def stringReduce(s: String): Int = {
    val x = greedy(s)
    val y = mathy(s)
    assert(x == y, "%s, %d == %d".format(s, x, y))
    println("Verified \"%s\" reduces to %d".format(s, x))
    x
  }

  def main(args: Array[String]) {
    stringReduce("bbaab")     // should be 1
    stringReduce("a")         // should be 1
    stringReduce("aaaaaaaa")  // should be 8
    stringReduce("aaaaaaab")  // should be 1
    stringReduce("abb")       // should be 1
    stringReduce("abbc")      // should be 1
    stringReduce("abc")       // should be 3
    stringReduce("abcb")      // should be 1
    stringReduce("abccccca")  // should be 1

    val r = new scala.util.Random
    while (true) {
      val s = (1 to r.nextInt(50) map (_ => (r.nextInt(3) + 'a').toChar)).mkString
      stringReduce(s)
    }
  }
}

