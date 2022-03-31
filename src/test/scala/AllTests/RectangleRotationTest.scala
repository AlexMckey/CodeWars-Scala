package AllTests

import org.scalatest._

import RectangleRotation.RectangleRotation as Kata

class RectangleRotationTest extends funsuite.AnyFunSuite {
  test("Basic tests") {
    assert(Kata.rectangleRotation(6, 4) === 23)
    assert(Kata.rectangleRotation(30, 2) === 65)
    assert(Kata.rectangleRotation(8, 6) === 49)
    assert(Kata.rectangleRotation(16, 20) === 333)
  }
}
