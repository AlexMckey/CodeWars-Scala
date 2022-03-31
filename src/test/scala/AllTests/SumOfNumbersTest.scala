package AllTests

import SumOfNumbers.*
import org.scalatest.*
import org.scalatest.matchers.*

class SumOfNumbersTest extends wordspec.AnyWordSpec with must.Matchers {
  "Sum#getSum" should {
    "return -1" in (SumOfNumbers.getSum(0, -1) mustBe -1)
    "return 1" in (SumOfNumbers.getSum(0, 1) mustBe 1)
  }
}
