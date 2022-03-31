package PerimeterOfSquares

import scala.annotation.tailrec
import scala.math

object PerimetrOfSquares {
  def perimeter(n: BigInt): BigInt = {
    @tailrec
    def fib(n: BigInt, a: BigInt = 1, b: BigInt = 1, sum: BigInt = 1): BigInt =
      if (n < 1) sum
      else fib(n - 1, b, a + b, sum + b)
    fib(n)*4
  }
  def perimeterBetter(n: BigInt): BigInt = {
    @tailrec
    def fib(n: BigInt, a: BigInt = 1, b: BigInt = 1): BigInt =
      if (n < 1) b - 1
      else fib(n - 1, b, a + b)
    fib(n)*4
  }

  def fib(n: Int): BigInt = {
    type Matrix = Array[Array[BigInt]]


    def mul(a: Matrix, b: Matrix): Matrix = {
      Array(
        Array(a(0)(0) * b(0)(0) + a(0)(1) * b(1)(0), a(0)(0) * b(0)(1) + a(0)(1) * b(1)(1)),
        Array(a(1)(0) * b(0)(0) + a(1)(1) * b(1)(0), a(1)(0) * b(0)(1) + a(1)(1) * b(1)(1)))
    }

    @scala.annotation.tailrec
    def pow(a: Matrix, b:Matrix, n: Long): Matrix ={
      if (n == 0) b
      else {
        if (n % 2 == 1) {
          pow(mul(a,a), mul(b,a), n/2)
        } else pow(mul(a,a), b, n/2)
      }
    }

    import math.BigInt._

    val B: Matrix = Array(Array(int2bigInt(1),int2bigInt(0)),Array(int2bigInt(0),int2bigInt(1)))
    val a: Matrix = Array(Array(int2bigInt(1),int2bigInt(1)),Array(int2bigInt(1),int2bigInt(0)))
    val b: Matrix = Array(Array(int2bigInt(0),int2bigInt(1)), Array(int2bigInt(1),int2bigInt(-1)))
    pow(a, B, n)(1)(0)
  }

  def perimeterClever(n: BigInt): BigInt = {
    (fib(n.toInt+3) - 1) * 4
  }
}
