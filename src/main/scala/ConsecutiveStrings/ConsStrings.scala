package ConsecutiveStrings

object ConsStrings {
  def longestConsec(strarr: Array[String], k: Int): String =
    if (strarr.isEmpty || k <= 0 || k > strarr.length) ""
    else strarr
      .sliding(k)
      .map(_.mkString)
      .maxBy(_.length)
//      .sliding(k)
//      .map(_.mkString(""))
//      .toList
//      .sortBy(_.length)(Ordering.Int.reverse)
//      .head
}
