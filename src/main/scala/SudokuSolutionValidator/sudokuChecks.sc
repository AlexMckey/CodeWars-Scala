val validBoard = Array(
  Array(5, 3, 4, 6, 7, 8, 9, 1, 2),
  Array(6, 7, 2, 1, 9, 5, 3, 4, 8),
  Array(1, 9, 8, 3, 4, 2, 5, 6, 7),
  Array(8, 5, 9, 7, 6, 1, 4, 2, 3),
  Array(4, 2, 6, 8, 5, 3, 7, 9, 1),
  Array(7, 1, 3, 9, 2, 4, 8, 5, 6),
  Array(9, 6, 1, 5, 3, 7, 2, 8, 4),
  Array(2, 8, 7, 4, 1, 9, 6, 3, 5),
  Array(3, 4, 5, 2, 8, 6, 1, 7, 9)
)

val valid = List(1,2,3,4,5,6,7,8,9)

validBoard.forall(_.sorted.sameElements(valid))
validBoard.transpose.forall(_.sorted.sameElements(valid))
validBoard
  .sliding(3,3)
  .flatMap(_.transpose
    .sliding(3,3)
    .map(_.flatten))
  .forall(_.sorted.sameElements(valid))
validBoard.zipWithIndex.map{ case (l,i) => l.drop(i).head }
  .sorted.sameElements(valid)
validBoard.zipWithIndex.map{ case (l,i) => l.drop(i-1).head }
  .sorted.sameElements(valid)