package SumOfTheFirst_nth_TermOfSeries

object SumNthSeries {
  def seriesSum(n: Int): String = {
    var sum = if (n == 0) 0.0 else 1.0
    if (n != 1)
      for (i <- 1 until n)
        sum += 1/(1.0 + i*3)
    f"$sum%1.2f".replace(',','.')
  }
}
