val hash = scala.collection.mutable.Set.empty[Long]
val mod = Seq(1,10,9,12,3,4)

implicit class CycleSeqs[T](val seq: Seq[T]) extends AnyVal {
  def cycle = LazyList.continually(seq).flatten
}
val mods = mod.cycle

mods.take(20)

var n = 1234567L
'1'.asDigit
val nr = n.toString.map(_.asDigit).reverseIterator
val n1 = nr.zip(mods).map(_*_).sum
val nr1 = n1.toString.map(_.asDigit).reverseIterator
val n2 = nr1.zip(mods).map(_*_).sum

var m = n
while (!hash.contains(m)) {
  hash.add(m)
  m = m.toString.map(_.asDigit).reverseIterator.zip(mods).map(_*_).sum
  println(m)
}
m

m = 1111111111
while (!hash.contains(m)) {
  hash.add(m)
  m = m.toString.map(_.asDigit).reverseIterator.zip(mods).map(_*_).sum
  println(m)
}
m