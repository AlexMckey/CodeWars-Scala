package ARuleOfDivisibilityBy13

import scala.annotation.tailrec

object RuleDivBy13 {
  private val mods = LazyList.continually(Seq(1,10,9,12,3,4)).flatten
  @tailrec
  def thirt(n: Long): Long = {
    val m = n.toString.map(_.asDigit).reverse.zip(mods)
      .map{ case (a,b) => a*b }.sum
    if (m == n) n else thirt(m)
  }
}
