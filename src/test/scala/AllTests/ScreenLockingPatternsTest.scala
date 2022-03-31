package AllTests

import org.scalatest._
import ScreenLockingPatterns.ScreenLockingPatterns as Kata

class ScreenLockingPatternsTest extends funsuite.AnyFunSuite {
  test("Basic tests") {
    assert(Kata.countPatternsFrom('A', 0) === 0)
    assert(Kata.countPatternsFrom('A', 10) === 0)
    assert(Kata.countPatternsFrom('B', 1) === 1)
    assert(Kata.countPatternsFrom('C', 2) === 5)
    assert(Kata.countPatternsFrom('D', 3) === 37)
    assert(Kata.countPatternsFrom('E', 4) === 256)
    assert(Kata.countPatternsFrom('E', 8) === 23280)
  }
}
