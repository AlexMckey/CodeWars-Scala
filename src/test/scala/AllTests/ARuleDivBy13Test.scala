package AllTests

import ARuleOfDivisibilityBy13.*
import AllTests.ARuleDivBy13Test.{dotest, *}
import org.scalatest.*
import org.scalatest.Assertions.*

class ARuleDivBy13Test extends flatspec.AnyFlatSpec {
  it should "pass basic tests" in {
    dotest(8529, 79)
    dotest(85299258, 31)
    dotest(5634, 57)
    dotest(1111111111, 71)

  }
}

object ARuleDivBy13Test {
  private def dotest(n: Long, expect: Long): Unit = {
    println("Testing: " + n)
    val actual: Long = RuleDivBy13.thirt(n)
    assertResult(expect){actual}
  }
}