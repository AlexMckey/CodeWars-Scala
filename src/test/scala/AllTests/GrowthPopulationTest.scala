package AllTests

import AllTests.GrowthPopulationTest.{testing, *}
import GrowthPopulation.*
import org.scalatest.*
import org.scalatest.Assertions.*

class GrowthPopulationTest extends flatspec.AnyFlatSpec {
  it should "pass Basic tests" in {
    testing(GrowthPopulation.nbYear, 1500, 5, 100, 5000, 15)
    testing(GrowthPopulation.nbYear, 1500000, 2.5, 10000, 2000000, 10)
    testing(GrowthPopulation.nbYearSimple, 1500, 5, 100, 5000, 15)
    testing(GrowthPopulation.nbYearSimple, 1500000, 2.5, 10000, 2000000, 10)
    testing(GrowthPopulation.nbYearRec, 1500, 5, 100, 5000, 15)
    testing(GrowthPopulation.nbYearRec, 1500000, 2.5, 10000, 2000000, 10)
  }
}

object GrowthPopulationTest {
  def testing(f: (Int, Double, Int, Int) => Int, p0: Int, percent: Double, aug: Int, p: Int, expect: Int): Unit = {
    val actual = f(p0, percent, aug, p)
    assertResult(expect){actual}
  }
}
