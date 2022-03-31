package AllTests

import AllTests.PlayingWithDigitsTest.{testing, *}
import PlayingWithDigits.DigPow
import org.scalatest.*
import org.scalatest.Assertions.*

class PlayingWithDigitsTest extends flatspec.AnyFlatSpec {
  it should "pass basic tests" in {
    testing(89, 1, 1)
    testing(92, 1, -1)
    testing(46288, 3, 51)
  }
}

object PlayingWithDigitsTest {
  private def testing(n: Int, p: Int, expect: Int): Unit = {
    println("Testing: " + n + ", " + p)
    val actual: Int = DigPow.digPow(n, p)
    println("Actual: " + actual)
    println("Expect: " + expect)
    println("*")
    assertResult(expect){actual}
  }
}