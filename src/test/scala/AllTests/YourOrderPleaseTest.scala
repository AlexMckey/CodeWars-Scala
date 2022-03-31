package AllTests

import YourOrderPlease.*
import org.scalatest.*
import org.scalatest.matchers.*

class YourOrderPleaseTest extends flatspec.AnyFlatSpec with should.Matchers {
  "order(\"is2 Thi1s T4est 3a\")" should "return \"Thi1s is2 3a T4est\"" in {
    ConcateInOrder.order("is2 Thi1s T4est 3a") should be ("Thi1s is2 3a T4est")
  }
}