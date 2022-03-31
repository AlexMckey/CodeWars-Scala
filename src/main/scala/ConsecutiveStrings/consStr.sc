val strarr = List("tree", "foling", "trashy", "blue", "abcdef", "uvwxyz")
val k = 2
strarr.sortBy(_.length)(Ordering.Int.reverse)
  .take(k).mkString("")
strarr.sliding(k).map(_.mkString("")).toList.sortBy(_.length)(Ordering.Int.reverse).headOption