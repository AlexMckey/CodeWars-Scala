import scala.annotation.tailrec

val p0 = 1500
val percent = 5
val aug = 100
val p = 5000

val res = Iterator.iterate((p0, 0)){ (x,y) =>
  ((x + x * percent / 100 + aug).toInt, y + 1)
}

res.dropWhile(_._1 < p).next

def nbYearRec(p0: Int, percent: Double, aug: Int, p: Int): Int = {
  @tailrec
  def rec(cur: Double, year: Int = 0): Int =
    if (cur > p) year
    else rec(cur + cur * percent / 100 + aug, year + 1)
  rec(p0)
}

nbYearRec(1000,2,50,1200)