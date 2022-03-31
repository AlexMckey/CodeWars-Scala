package AllTests

import org.scalatest._
import org.scalatest.Assertions._

import GettingAlongWithIntegerPartitions.IntsPart._
import GettingAlongWithIntegerPartitionsTest._

class GettingAlongWithIntegerPartitionsTest extends flatspec.AnyFlatSpec {
  it should "pass basic tests" in {
    dotest(1, "Range: 0 Average: 1.00 Median: 1.00")
    dotest(4, "Range: 3 Average: 2.50 Median: 2.50")
    dotest(16, "Range: 323 Average: 84.44 Median: 56.00")
  }
}

object GettingAlongWithIntegerPartitionsTest {
  private def dotest(n: Long, expect: String): Unit = {
    println("Testing: " + n)
    val actual: String = part(n)
    println("Actual: " + actual)
    println("Expect: " + expect)
    println("*")
    assertResult(expect){actual}
  }
}