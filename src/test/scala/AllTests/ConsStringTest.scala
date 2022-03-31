package AllTests

import AllTests.ConsStringTest.{testing, *}
import ConsecutiveStrings.*
import org.scalatest.*
import org.scalatest.Assertions.*

class ConsStringTest extends flatspec.AnyFlatSpec {
  it should "pass basic tests" in {
    testing(Array("zone", "abigail", "theta", "form", "libe", "zas", "theta", "abigail"), 2, "abigailtheta")
    testing(Array("ejjjjmmtthh", "zxxuueeg", "aanlljrrrxx", "dqqqaaabbb", "oocccffuucccjjjkkkjyyyeehh"), 1, "oocccffuucccjjjkkkjyyyeehh")
    testing(Array(), 3, "")

  }
}

object ConsStringTest {
  private def testing(s: Array[String], k: Int, expect: String): Unit = {
    println("Testing: " + s.mkString(",") + ", " + k)
    val actual: String = ConsStrings.longestConsec(s, k)
    println("Actual: " + actual)
    println("Expect: " + expect)
    println("-")
    assertResult(expect){actual}
  }
}
