implicit class CycleIterableOps[A](coll: Iterable[A]) {
  def cycle: Iterator[A] = {
    // https://stackoverflow.com/a/2099896
    Iterator.continually(coll).flatten
  }
}
val in = List(
  List(1, 2, 3),
  List(4, 5, 6),
  List(7, 8, 9))

val n = in.size

val idxs = List((0,1),(1,0),(0,-1),(-1,0)).cycle
val cnts = (n-1) +: Range(n-1,0,-1).flatMap(i => List.fill(2)(i))
val ds = cnts.zip(idxs).flatMap((n,i) => List.fill(n)(i))
val res = ds.scan((0,0)){case ((x0,y0),(dx,dy)) => (x0+dx,y0+dy)}
res.map((i,j) => in(i)(j))