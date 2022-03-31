package AllTests

import org.scalatest._

import Psychic.Psychic.guess

class PsychicTest extends flatspec.AnyFlatSpec with matchers.should.Matchers {
  "The Psychic" should "guess correctly" in {
    guess() shouldBe java.lang.Math.random()
  }
}