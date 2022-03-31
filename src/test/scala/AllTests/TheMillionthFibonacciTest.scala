package AllTests

import org.scalatest._

import TheMillionthFibonacci.TheMillionthFibonacci._

object mf {
  def myFib(n: Int): BigInt = {
    var nn:   Int = n
    var sign: Int = 0
    if (nn < 0) {
      nn = -nn
      sign = 2 * (nn % 2) - 1
    } else {
      sign = 1
    }
    var a: BigInt = 1
    var b: BigInt = 0
    var p: BigInt = 0
    var q: BigInt = 1
    while (nn != 0) {
      if (nn % 2 == 1) {
        var t  = (b + a) * q + a * p
        b = b * p + a * q
        a = t
      }
      var t = p * p + q * q
      q =(2 * p * q) + q * q
      p = t
      nn /= 2
    }
    sign * b
  }
}

class TheMillionthFibonacciTest extends flatspec.AnyFlatSpec with matchers.should.Matchers {
  "fib(0)" should "return 0" in {
    fib(0) should be (0)
  }
  "fib(1)" should "return 1" in {
    fib(1) should be (1)
  }
  "fib(2)" should "return 1" in {
    fib(2) should be (1)
  }
  "fib(3)" should "return 2" in {
    fib(3) should be (2)
  }
  "fib(4)" should "return 4" in {
    fib(4) should be (3)
  }
  "fib(5)" should "return 5" in {
    fib(5) should be (5)
  }
  "fib(-6)" should "return -8" in {
    fib(-6) should be (-8)
  }
  "fib(-96)" should "return -51680708854858323072" in {
    fib(-96).toString should be ("-51680708854858323072")
  }
//  (1 to 10).foreach { i =>
//    val x = scala.util.Random
//    val t = -x.nextInt(100)
//    val ans3 = mf.myFib(t)
//    s"fib($t)" should s"return $ans3" in {
//      fib(t).toString should be (s"$ans3")
//    }
//  }
}
