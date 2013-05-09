/**
 * You are given a list of uniformly random coin flips (0 and 1).
 * Generate as large a list as possible of uniformly random dice rolls
 * (0 through 5).
 *
 * The solution here is a special case of arithmetic encoding.  We start with
 * the range [0, 6).  Each coin flip determines one half of the range.  Once
 * the range is entirely contained within a single integer value, the dice
 * roll is determined.  At which point, we multiply the range by 6 and
 * recenter the lower boundary at 0.  Since we need perfect accuracy, and
 * since every division is by 2, we could use rationals, but instead, we
 * simply track the denominator as 2^k and each numerator for the range.
 */

import scala.util.Random

object DiceFromCoins {
  def bits(): Stream[Int] = {
    val r = new Random()
    def f(): Stream[Int] = r.nextInt(2) #:: f()
    f()
  }

  def dice(bits: Stream[Int]): Stream[Int] = {
    def f(lo: BigInt, hi: BigInt,
	  k: Int, bits: Stream[Int]): Stream[Int] = {
      val x = lo >> k
      val y = (hi - 1) >> k
      if (x == y) {
	val new_lo = 3 * (lo - (x << k))
	val new_hi = 3 * (hi - (x << k))
	x.toInt #:: f(new_lo, new_hi, k - 1, bits)
      } else {
	val m = lo + hi
	bits.head match {
	  case 0 => f(lo << 1, m, k + 1, bits.tail)
	  case 1 => f(m, hi << 1, k + 1, bits.tail)
	}
      }
    }
    f(0, 6, 0, bits)
  }

  def main(args: Array[String]) {
    val rolls = dice(bits()).take(10000).toList
    val hist = rolls.sorted.groupBy(identity _).mapValues { _.length }
    println(hist)
  }
}

