package myLib

import Primes._

object Primes {
//  def isPrime(candidate: Int): Boolean = {
//    if (candidate == 2) true
//    else if (candidate%2 == 0) false
//    else (3 to Math.sqrt(candidate).toInt by 2).forall(d => candidate%d != 0)
//  }
  
  implicit class RichInt(self: Int){
    def isPrime: Boolean = {
      if (self <= 3) return true
      else if (self % 2 == 0) return false
      for (i <- 2 to math.sqrt(self).toInt + 1) {
        if (self % i == 0) return false
      }
      true
    }
    def nextPrime(): Int = {
      LazyList.from(if (self % 2 == 0) self+1 else self+2,2).dropWhile(!_.isPrime).head
    }
  }

  implicit class RichLong(self: Long) {
    def isPrime: Boolean = {
      if (self <= 3) return true
      else if (self % 2 == 0) return false
      for (i <- 2 to math.sqrt(self.toDouble).toInt + 1) {
        if (self % i == 0) return false
      }
      true
    }

    def nextPrime: Long = {
      var i = if (self % 2 == 0) self + 1 else self + 2
      while (!i.isPrime) i += 2
      i
    }
  }
}