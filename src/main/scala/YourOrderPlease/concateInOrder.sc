val s1 = "is2 Thi1s T4est 3a"
val s2 = "4of Fo1r pe6ople g3ood th5e the2"

s1.split(' ').map(_.partition(_.isDigit)).sortBy(_._1).map(_._2).mkString(" ")
s2.split(' ').map(_.partition(_.isDigit)).sortBy(_._1).map(_._2).mkString(" ")
s1.split(' ').sortBy(_.find(_.isDigit)).mkString(" ")