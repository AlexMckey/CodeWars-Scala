package PlayingWithDigits

object DigPow {
  def digPow(n: Int, p: Int): Int = {
    val s = n.toString.map(_ - '0')
    val l = p until p+s.size
    val sum: Double = s.zip(l)
      .map{case(x,p) => Math.pow(x.toFloat,p)}
      .sum
    if ((sum / n).isWhole)
      (sum / n).intValue
    else -1
  }

  def digPowPerfect(n: Int, p: Int): Int =
    n.toString.zipWithIndex
      .map { case (d, i) => math.pow(d.asDigit, i+p) }
      .sum / n match {
        case r if r.isWhole => r.toInt
        case _              => -1
      }
}
