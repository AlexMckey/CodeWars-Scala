import scala.annotation.tailrec

def fibRec(n: Int) = {
  @tailrec
  def recPlus(k: Int, a: Int = 1, b: Int = 0): Int = {
    if (k == 0) b
    else recPlus(k - 1, a + b, a)
  }

  @tailrec
  def recMinus(k: Int, a: Int = 1, b: Int = 0): Int = {
    if (k == 0) a
    else recMinus(k + 1, a - b, a)
  }

  if (n < 0) recMinus(n)
  else recPlus(n)
}

fibRec(0)
fibRec(1)
fibRec(2)
fibRec(5)
fibRec(-1)
fibRec(-2)
fibRec(-3)
fibRec(-4)
fibRec(-5)

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

-8 % 2
-7 % 2

fib(0)
fib(1)
fib(2)
fib(3)
fib(4)
fib(5)
fib(-1)
fib(-2)
fib(-3)
fib(-4)
fib(-5)
fib(-6)
