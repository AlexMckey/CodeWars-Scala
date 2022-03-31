//val n = 7
//val ls = Range(1,n+1).flatMap(i => List.fill(n / i)(i))
//val cs = Range(1,n+1).flatMap(c => ls.combinations(c).filter(_.sum == n))
//val rs = cs.map(_.product).distinct.sorted
//rs.last - rs.head
//1.0 * rs.sum / rs.size
//if (rs.size % 2 == 1)
//  rs(rs.size / 2) * 1.0
//else
//  (rs(rs.size / 2) + rs(rs.size / 2 - 1)) / 2.0
val d = 1.5
f"$d%.2f"
def memoizeFnc[K, V](f: K => V): K => V = {
  val cache = collection.mutable.Map.empty[K, V]

  k =>
    cache.getOrElse(k, {
      cache update(k, f(k))
      cache(k)
    })
}

def part(n: Int): List[List[Int]] = {
  var res = List(List(n))
  for (i <- 1 until n) {
    val l = n - i
    part(i).filter(_.head <= l)
      .foreach(r => res = res.appended(r.appended(l)))
  }
  res
}

val memoPart = memoizeFnc(part)

val res = memoPart(7)
res.map(_.product).distinct.sorted

import scala.annotation.tailrec
type Sum = List[Int]

def sums(n: Int): Iterator[Sum] = {
  /** Prepends the head of the stack repeatedly until a total amount has
   * been added.  The leading element of the result is then increased
   * so that the total added is exactly the specified amount.
   */
  @tailrec
  def topUp(amount: Int, stack: List[Int]): List[Int] = {
    if (amount < stack.head)
      (stack.head + amount) :: stack.tail
    else
      topUp(amount - stack.head, stack.head :: stack)
  }

  /** Returns the next partition by popping the first element, increasing the
   * next element, and topping it up to the same total.  Returns None if
   * the input has only one element.
   */
  def advance(partition: Option[Sum]): Option[Sum] = {
    partition.get match {
      case List(n) => None
      case p       => Some(topUp(p.head - 1, p.tail.head + 1 :: p.tail.tail))
    }
  }

  Iterator.iterate(Some(List.fill(n)(1)): Option[Sum])(advance)
    .takeWhile(_.isDefined)
    .map(_.get)
}

sums(5).map(_.product).distinct.toList.sorted