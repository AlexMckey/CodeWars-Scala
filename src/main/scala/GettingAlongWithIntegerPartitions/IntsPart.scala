package GettingAlongWithIntegerPartitions

object IntsPart {
//  private def memoizeFnc[K, V](f: K => V): K => V = {
//    val cache = collection.mutable.Map.empty[K, V]
//
//    k =>
//      cache.getOrElse(k, {
//        cache update(k, f(k))
//        cache(k)
//      })
//  }
//
//  private def partLists(n: Long): List[List[Long]] = {
//    var res = List(List(n))
//    for (i <- 1 until n.toInt) {
//      val l = n - i
//      partLists(i).filter(_.head <= l)
//        .foreach(r => res = res.appended(r.appended(l)))
//    }
//    res
//  }
//
//  def part(n: Long): String = {
//    val memoPart = memoizeFnc(partLists(_))
//    val res = memoPart(n).map(_.product).distinct.sorted
//    val p = res.last - res.head
//    val a = 1.0 * res.sum / res.size
//    val m = if (res.size % 2 == 1)
//      res(res.size / 2) * 1.0
//    else
//      (res(res.size / 2) + res(res.size / 2 - 1)) / 2.0
//    f"Range: $p Average: $a%.2f Median: $m%.2f".replace(',','.')
//  }

  import scala.annotation.tailrec
  type Sum = List[Long]

  def part(n: Long): String = {
    @tailrec
    def topUp(amount: Long, stack: List[Long]): List[Long] = {
      if (amount < stack.head)
        (stack.head + amount) :: stack.tail
      else
        topUp(amount - stack.head, stack.head :: stack)
    }
    def advance(partition: Option[Sum]): Option[Sum] = {
      partition.get match {
        case List(n) => None
        case p       => Some(topUp(p.head - 1, p.tail.head + 1 :: p.tail.tail))
      }
    }

    val res = Iterator.iterate(Some(List.fill(n.toInt)(1)): Option[Sum])(advance)
      .takeWhile(_.isDefined)
      .map(_.get).map(_.product).distinct.toList.sorted
    val p = res.last - res.head
    val a = 1.0 * res.sum / res.size
    val m = if (res.size % 2 == 1)
      res(res.size / 2) * 1.0
    else
      (res(res.size / 2) + res(res.size / 2 - 1)) / 2.0
    f"Range: $p Average: $a%.2f Median: $m%.2f".replace(',','.')
  }

  def partBetter(n: Long): String = {
    var set: Set[Long] = Set()
    def par(target: Long, max: Long, cur: List[Long]): Unit = {
      if (target == 0)
        set += cur.product
      else if (target > 0) {
        for (i <- 1L to max) {
          par(target - i, i, i :: cur)
        }
      }
    }
    par(n, n, List())

    val res = set.toList.sorted
    val rn = res.length

    val range = res.max - res.min
    val averge = res.sum / rn.toDouble
    val median = (res((rn-1)/2) + res(rn/2)) / 2.0
    f"Range: $range Average: $averge%.2f Median: $median%.2f"
  }

  def partClever(n: Long): String = {
    def enumProducts(n: Long, max: Long = Long.MaxValue, prod: Long = 1): Seq[Long] =
      (2L to Math.min(n, max)).flatMap(i => Seq(prod * i) ++ enumProducts(n - i, i, prod * i))
    val result = 1L +: enumProducts(n).distinct.sorted
    val range = result.last - result.head
    val average = result.sum.toDouble / result.length
    val median = (result(result.length / 2) + result(result.length - result.length / 2 - 1)) / 2.0
    f"Range: $range Average: $average%.2f Median: $median%.2f"
  }
  val prod: LazyList[List[Long]] = {
    Nil #:: LazyList.from(1).map { x =>
      (x :: (1 until x)
        .flatMap { b =>
          prod(x - b).map(a => a * b)
        }.toList : List[Long]).distinct
    }
  }

}
