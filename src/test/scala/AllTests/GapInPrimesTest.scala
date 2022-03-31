package AllTests

import AllTests.GapInPrimesTest.{doBItest, dotest, *}
import GapInPrimes.*
import org.scalatest.*
import org.scalatest.Assertions.*

import java.util.Random

object GapInPrimesTest {
  private def dotest(g: Int, m: Long, n: Long, expect: String): Unit = {
    val actual: String = GapPrimes.gap(g, m, n)
    assertResult(expect){actual}
  }
  private def doBItest(g: Int, m: Long, n: Long, expect: String): Unit = {
    val actual: String = GapPrimes.gap(g, m, n)
    assertResult(expect){actual}
  }
}

class GapInPrimesTest extends flatspec.AnyFlatSpec {
  it should "pass basic tests" in {
    dotest(2, 100, 100, "")
    dotest(2, 100, 110, "(101,103)")
    dotest(4, 100, 110, "(103,107)")
    dotest(8, 340, 400, "(359,367)")
    dotest(2,2308,2400, "(2309,2311)")
  }
  it should "pass basic BI tests" in {
    doBItest(2, 100, 100, "")
    doBItest(2, 100, 110, "(101,103)")
    doBItest(4, 100, 110, "(103,107)")
    doBItest(8, 340, 400, "(359,367)")
    doBItest(2,2308,2400, "(2309,2311)")
  }
}
