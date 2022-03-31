package GrowthPopulation

import scala.annotation.tailrec

object GrowthPopulation {
  def nbYear(p0: Int, percent: Double, aug: Int, p: Int): Int = {
    Iterator.iterate((p0, 0)){ (x,y) =>
      ((x + x * percent / 100 + aug).round.toInt, y + 1)}
      .dropWhile(_._1 < p).next._2
  }

  def nbYearSimple(p0: Int, percent: Double, aug: Int, p: Int): Int =
    Iterator.iterate(p0.toDouble)( x =>
      (x + x * percent / 100 + aug).floor).takeWhile(_ < p).size

  def nbYearRec(p0: Int, percent: Double, aug: Int, p: Int): Int = {
    @tailrec
    def rec(cur: Double, year: Int = 0): Int =
      if (cur > p) year
      else rec(cur + cur * percent / 100 + aug, year + 1)
    rec(p0)
  }
}
