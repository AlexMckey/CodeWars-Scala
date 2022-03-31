package AllTests

import org.scalatest.*

import scala.util.Random

class PerimeterOfSquaresTest extends funspec.AnyFunSpec {

  import PerimeterOfSquares.PerimetrOfSquares.*

  describe("Example Test Cases") {
    val testCases = Array(
      (5, BigInt(80)), (7, BigInt(216)), (20, BigInt(114624)), (30, BigInt(14098308)),
      (100, BigInt("6002082144827584333104")),
    )

    for( (n, r) <- testCases ) {
      check(n, r)
      checkClever(n, r)
    }
  }
  def check(n: BigInt, res: BigInt): Unit = {
    val z = perimeter(n)
    it(s"perimeter($n) should be equal to $res") {
      assert(z === res)
    }
  }
  def checkClever(n: BigInt, res: BigInt): Unit = {
    val v = perimeterClever(n)
    it(s"perimeterClever($n) should be equal to $res") {
      assert(v === res)
    }
  }
}
