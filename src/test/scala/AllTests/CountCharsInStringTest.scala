package AllTests

import CountCharactersInYourString.*
import org.scalatest.funsuite.AnyFunSuite

class CountCharsInStringTest extends AnyFunSuite {
  test("Count Chars in String - Simples") {
    assert(CountCharsInString.count("aba") == Map[Char, Int]('a' -> 2, 'b' -> 1))
    assert(CountCharsInString.count("") == Map[Char, Int]())
  }
}
