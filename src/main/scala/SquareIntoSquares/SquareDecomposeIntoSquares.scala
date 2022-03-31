package SquareIntoSquares

import scala.annotation.tailrec

object SquareDecomposeIntoSquares {
  def decompose(n: Long): String = {
    var goal = 0L
    var res = List(n)
    while (res.nonEmpty) {
      val cur = res.head
      res = res.tail
      goal += cur * cur
      for (i <- cur - 1 to 1 by -1) {
        if (goal - (i * i) >= 0) {
          goal -= i * i
          res = i +: res
          if (goal == 0)
            return res.sorted.mkString(" ")
        }
      }
    }
    null
  }

  def decomposeClever(n: Long): String = {
    def decomp(n: Long, left: Long, chain: List[Long]): Option[List[Long]] =
      left - n * n match {
        case x if x == 0 => Option(n :: chain)
        case x if x > 0 && n > 1 => decomp(n - 1, x, n :: chain).orElse(decomp(n - 1, left, chain))
        case x if x < 0 && n > 1 => decomp(n - 1, left, chain)
        case _ => None
      }
    decomp(n - 1, n * n, List()).map(l => l.mkString(" ")).orNull
  }
}
