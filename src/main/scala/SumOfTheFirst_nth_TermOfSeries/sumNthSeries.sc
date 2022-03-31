def seriesSum(n: Int): String =
  (0 until n).foldLeft(0.0)((acc,i) => acc + 1/(1.0 + i*3))
    .formatted("%1.2f")

seriesSum(1)
seriesSum(2)
seriesSum(3)
seriesSum(5)