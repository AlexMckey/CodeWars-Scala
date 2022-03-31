package SnailSort

object SnailSort {
  implicit class CycleIterableOps[A](coll: Iterable[A]) {
    def cycle: Iterator[A] = Iterator.continually(coll).flatten
  }
  def snail(xs: List[List[Int]]): List[Int] = {
    if (xs.isEmpty || xs.head.isEmpty) return List.empty
    val n = xs.size
    val idxs = List((0,1),(1,0),(0,-1),(-1,0)).cycle
    val cnts = (n-1) +: Range(n-1,0,-1).flatMap(i => List.fill(2)(i))
    val ds: IndexedSeq[(Int,Int)] = cnts.zip(idxs).flatMap((n, i) => List.fill(n)(i))
    val res = ds.scan((0,0)){case ((x0,y0),(dx,dy)) => (x0+dx,y0+dy)}
    res.map{ case (i,j) => xs(i)(j) }.toList
  }

  def snailClever(xs: List[List[Int]]): List[Int] = xs match {
    case Nil => Nil
    case x :: rest => x ++ snail(rest.transpose.reverse)
  }
}
