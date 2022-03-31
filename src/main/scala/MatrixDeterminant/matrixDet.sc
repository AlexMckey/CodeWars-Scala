val a1 = Array(1)
val d1 = a1.head
val a2 = Array(Array(1,3),Array(2,5))
val d2 = a2(0)(0)*a2(1)(1) - a2(1)(0)*a2(0)(1)
val a3 = Array(Array(2, 5, 3), Array(1, -2, -1), Array(1, 3, 4))
val d3 = a3(0)(0)*a3(1)(1)*a3(2)(2) +
         a3(0)(1)*a3(1)(2)*a3(2)(0) +
         a3(0)(2)*a3(1)(0)*a3(2)(1) -
         a3(0)(2)*a3(1)(1)*a3(2)(0) -
         a3(0)(0)*a3(1)(2)*a3(2)(1) -
         a3(0)(1)*a3(1)(0)*a3(2)(2)
def minor(a: Array[Array[Int]], i: Int, j: Int): Array[Array[Int]] = {
  a.take(i).map(v => v.take(j) ++ v.drop(j+1)) ++ a.drop(i+1).map(v => v.take(j) ++ v.drop(j+1))
}
def det(a: Array[Array[Int]]): Int = a.length match {
  case 1 => a.head.head
  case 2 => a(0)(0)*a(1)(1) - a(1)(0)*a(0)(1)
//  case 3 => a(0)(0)*a(1)(1)*a(2)(2) +
//    a(0)(1)*a(1)(2)*a(2)(0) +
//    a(0)(2)*a(1)(0)*a(2)(1) -
//    a(0)(2)*a(1)(1)*a(2)(0) -
//    a(0)(0)*a(1)(2)*a(2)(1) -
//    a(0)(1)*a(1)(0)*a(2)(2)
  case _ => a(0).zipWithIndex.map((v,i) => v * (if (i % 2 == 0) 1 else -1) * det(minor(a,0,i))).sum
}
det(a3)
val n = 0
val k = 1
a3.take(n).map(a => a.take(k) ++ a.drop(k+1)) ++ a3.drop(n+1).map(a => a.take(k) ++ a.drop(k+1))