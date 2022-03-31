package AllTests

import ValidBraces.*
import org.scalatest.funsuite.AnyFunSuite

class ValidBracesTest extends AnyFunSuite {
  test("ValidBraces - Simples") {
    assert(ValidBraces.validBraces("()"))
    assert(!ValidBraces.validBraces("[(])"))
  }
  test("ValidVraces - Harderst") {
    assert(ValidBraces.validBraces("[([][[][{}[]]])]"))
    assert(ValidBraces.validBraces("({[[[]{[]}()]]})"))
    assert(ValidBraces.validBraces("{[][[()[({()})]]]}"))
    assert(!ValidBraces.validBraces("[)))]]]]{"))
    assert(ValidBraces.validBracesSimple("{[][[()[({()})]]]}"))
    assert(!ValidBraces.validBracesSimple("[)))]]]]{"))
  }
}