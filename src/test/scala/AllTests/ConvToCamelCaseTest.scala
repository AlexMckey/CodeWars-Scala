package AllTests

import ConvertStringToCamelCase.*
import org.scalatest.*
import org.scalatest.matchers.*

class ConvToCamelCaseTest extends flatspec.AnyFlatSpec with must.Matchers {
  "the_Stealth_Warrior underscore lower start" should "theStealthWarrior" in {
    assert(ConvToCamelCase.toCamelCase("the_Stealth_Warrior") === "theStealthWarrior")
  }

  "the-Stealth-Warrior test dash " should "theStealthWarrior" in {
    assert(ConvToCamelCase.toCamelCase("the-Stealth-Warrior") === "theStealthWarrior")
  }
}
