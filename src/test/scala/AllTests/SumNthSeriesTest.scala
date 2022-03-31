package AllTests

import SumOfTheFirst_nth_TermOfSeries.*
import org.scalatest.*
import org.scalatest.matchers.*

class SumNthSeriesTest extends flatspec.AnyFlatSpec with should.Matchers {
  val tests = List(
    (1, "1.00"),
    (2, "1.25"),
    (3, "1.39")
  )
  tests.foreach {
    case (input, expected) =>
      s"seriesSum($input)" should s"return $expected" in {
        SumNthSeries.seriesSum(input) should be (expected)
      }
  }
}
