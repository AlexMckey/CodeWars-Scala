package SumOfNumbers

object SumOfNumbers {
  def getSum(a: Int, b: Int): Int = {
    (math.min(a,b) to math.max(a,b)).sum
  }

  def getSumBetter(a: Int, b: Int): Int =
    ((a - b).abs + 1) * (a + b) / 2
}
