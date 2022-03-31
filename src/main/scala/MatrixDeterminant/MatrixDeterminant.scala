package MatrixDeterminant

object MatrixDeterminant {
  def determinant(matrix: Array[Array[Int]]): Int = {
    def minor(a: Array[Array[Int]], i: Int, j: Int): Array[Array[Int]] = {
      a.take(i).map(v => v.take(j) ++ v.drop(j+1)) ++ a.drop(i+1).map(v => v.take(j) ++ v.drop(j+1))
    }
    matrix.length match {
      case 1 => matrix.head.head
      case 2 => matrix(0)(0)*matrix(1)(1) - matrix(1)(0)*matrix(0)(1)
        case 3 => matrix(0)(0)*matrix(1)(1)*matrix(2)(2) +
          matrix(0)(1)*matrix(1)(2)*matrix(2)(0) +
          matrix(0)(2)*matrix(1)(0)*matrix(2)(1) -
          matrix(0)(2)*matrix(1)(1)*matrix(2)(0) -
          matrix(0)(0)*matrix(1)(2)*matrix(2)(1) -
          matrix(0)(1)*matrix(1)(0)*matrix(2)(2)
      case _ => matrix(0).zipWithIndex.map((v,i) => v * (if (i % 2 == 0) 1 else -1) * determinant(minor(matrix,0,i))).sum
    }
  }

  def detClever(matrix: Array[Array[Int]]): Int =
    matrix match {
      case Array(Array(a)) => a
      case _ =>
        matrix
          .head.zipWithIndex
          .map { case (x, i) => math.pow(-1, i).toInt * x * determinant(matrix.drop(1).map(_.patch(i, Nil, 1))) }
          .sum
    }
}
