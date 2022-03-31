package GapInPrimes

object GapPrimes {
  def isPrime(n: Long): Boolean = {
    if (n == 2) true
    else if (n % 2 == 0) false
    else (3 to math.sqrt(n.toDouble).toInt by 2)
      .forall(n % _ != 0)
  }
  def nextPrime(n: Long): Long = {
    var i = n + n % 2 + 1
    while (!isPrime(i)) i += 2
    i
  }
  def gap(g: Int, m: Long, n: Long): String = {
    val p = (m to n-g)
      .dropWhile{ x =>
        val isP = isPrime(x)
        !isP || (isP && nextPrime(x) - x != g)
      }.headOption
    p match {
      case Some(v) => (v, v + g).toString()
      case _ => ""
    }
  }
  def gapBI(g: Int, m: Long, n: Long): String = {
    BigInt(m).to(n).filter(_.isProbablePrime(4))
      .sliding(2)
      .find { case IndexedSeq(a,b) => b - a == g }
      .map {  case IndexedSeq(a,b) => (a,b).toString }
      .getOrElse("")
  }
}
