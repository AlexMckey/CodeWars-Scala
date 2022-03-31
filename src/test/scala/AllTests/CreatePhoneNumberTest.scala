package AllTests

import CreatePhoneNumber.*
import org.scalatest.*
import org.scalatest.matchers.*

class CreatePhoneNumberTest extends flatspec.AnyFlatSpec with should.Matchers {
  List(
    (Seq(1, 2, 3, 4, 5, 6, 7, 8, 9, 0), "(123) 456-7890"),
    (Seq(1, 1, 1, 1, 1, 1, 1, 1, 1, 1), "(111) 111-1111")
  ).foreach {
    case (numbers, exp) =>
      s"createPhoneNumber($numbers)" should s"return $exp" in {
        CreatePhoneNumber.createPhoneNumber(numbers) shouldBe exp
      }
  }
}
