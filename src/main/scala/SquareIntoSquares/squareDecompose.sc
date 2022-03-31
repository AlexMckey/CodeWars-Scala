import scala.annotation.tailrec

val n = 11
val n2 = n*n
val r = Range(n-1,0,-1)//.map(a => a*a)
@tailrec
def rec(t: IndexedSeq[Int], s: Int, acc: List[Int] = List.empty): List[Int] = {
//  println(t)
//  println(s)
  if (t.isEmpty && s != 0) List.empty
  else if (s == 0) acc
  else if (t.head * t.head > s) rec(t.tail, s, acc)
  else rec(t.tail, s - t.head * t.head, acc :+ t.head)
}
rec(r, n * n) // True
rec(Range(11,0,-1),144) // True
rec(Range(50-1,0,-1),50*50)
rec(Range(625-1,0,-1),625*625)
rec(Range(7100-1,0,-1),7100*7100)

def decompose(n: Int): List[Int] = {
  var goal = 0
  var res = List(n)
  while (res.nonEmpty) {
    val cur = res.head
    res = res.tail
    goal += cur * cur
    for (i <- cur - 1 to 1 by -1) {
      if (goal - (i * i) >= 0) {
        goal -= i * i
        res = i +: res
        if (goal == 0)
          return res.sorted
      }
    }
  }
  Nil
}

decompose(12)
decompose(11)
decompose(625)