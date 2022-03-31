package TheMillionthFibonacci

import scala.annotation.tailrec

object TheMillionthFibonacci {
  def I(n: Int): Array[Array[BigInt]] = {
    val res = Array.fill(n)(Array.fill(n)(BigInt(0)))
    res.indices.foreach(i => res(i)(i) = 1)
    res
  }

  def matrix_multiply(A: Array[Array[BigInt]], B: Array[Array[BigInt]]): Array[Array[BigInt]] = {
    A.map(ar => B.transpose.map(bc => ar.zip(bc).map((a, b) => a * b).sum))
  }

  def pow(x: Array[Array[BigInt]], n: Int): Array[Array[BigInt]] = n match {
    case 0 => I(2)
    case 1 => x
    case _ =>
      var y = pow(x, n / 2)
      y = matrix_multiply(y,y)
      if (n % 2 == 1)
        y = matrix_multiply(x,y)
      y
  }

  def fib(n: Int): BigInt = {
    val res = if (n < 0) pow(Array(Array[BigInt](1, 1), Array[BigInt](1, 0)), -n)(0)(1)
    else pow(Array(Array[BigInt](1, 1), Array[BigInt](1, 0)), n)(0)(1)
    val k = if (n < 0 && n % 2 == 0) -1 else 1
    res * k
  }
}

object Fib {
  def fibs(n: Int): (BigInt, BigInt) = n match {
    case 0 => (0, 1)
    case 1 => (1, 0)
    case _ =>
      val (a, b) = fibs(n / 2)
      val p = (2 * b + a) * a
      val q = a * a + b * b
      if (n % 2 == 0) (p, q) else (p + q, p)
  }

  def fib(n: Int): BigInt =
    fibs(n.abs)._1 * (if (n < 0 && n % 2 == 0) -1 else 1)
}

object FibOther {

  import scala.math.{signum => s}

  def fib(n: Int): BigInt = n match {
    case 0 => BigInt(0)
    case 1 | -1 => BigInt(1)
    case _ if n % 2 == 0 => (n / 2).abs match { case k =>
      fib(k) match { case fk => s(n) * fk * (2 * fib(k - 1) + fk) }
    }
    case _ => (n + 1) / 2 match { case k =>
      fib(k).pow(2) + fib(k - 1).pow(2)
    }
  }
}
