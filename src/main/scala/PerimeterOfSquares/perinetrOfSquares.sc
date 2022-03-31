import scala.annotation.tailrec

@tailrec
def fib(n: Int, a: Int = 1, b: Int = 1, sum: Int = 1): Int =
  if (n < 1) a
  else fib(n-1, b, a+b, sum + b)

fib(1)
fib(2)
fib(3)
fib(4)
fib(5)
fib(6)