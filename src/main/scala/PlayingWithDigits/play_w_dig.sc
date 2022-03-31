val n1 = 89
val n2 = 695
val n3 = 46288
val n4 = 92

val r = 1
val c = 2
val l = r until r+c
val x = n4.toString.map(_ - '0').zip(l)
  .map((x,p) =>Math.pow(x,p)).sum
val a = x / n4
a.isWhole
51.0.isWhole

def digPow(n: Int, p: Int): Int = {
  val s = n.toString.map(_ - '0')
  val l = p until p+s.size
  val sum: Double = s.zip(l)
    .map((x,p) =>Math.pow(x,p))
    .sum
  if ((sum / n).isWhole)
    (sum / n).intValue
  else -1
}

digPow(89, 1)
digPow(92, 1)
digPow(695, 2)
digPow(46288, 3)