package AllTests

import org.scalatest._
import RGBToHexConversion.RGBToHex._

class RGBToHexConversionTest extends funsuite.AnyFunSuite {

  test("Samples") {
    assert(rgb(0, 0, 0) === "000000")
    assert(rgb(1, 2, 3) === "010203")
    assert(rgb(255, 255, 255) === "FFFFFF")
    assert(rgb(254, 253, 252) === "FEFDFC")
    assert(rgb(-20, 275, 125) === "00FF7D")
  }
}
