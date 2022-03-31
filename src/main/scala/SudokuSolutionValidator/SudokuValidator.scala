package SudokuSolutionValidator

object SudokuValidator {
  def isValid(board: Array[Array[Int]]): Boolean = {
    val valid = (1 to 9).toSet
    board.forall(_.toSet == valid) &&
    board.transpose.forall(_.toSet == valid) &&
    board.grouped(3).forall(
      _.transpose
        .grouped(3)
        .forall(_.flatten.toSet == valid))
  }
}
