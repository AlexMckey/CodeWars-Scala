package RectangleRotation

object RectangleRotation {
  def rectangleRotation(a: Int, b: Int): Int = {
    val halfH = (a / math.sqrt(2)) / 2
    val halfW = (b / math.sqrt(2)) / 2
    val h1 = halfH.floor.toInt * 2 + 1
    val w1 = halfW.floor.toInt * 2 + 1

    val h2 = h1 + (if (halfH - halfH.floor < 0.5) -1 else 1)
    val w2 = w1 + (if (halfW - halfW.floor < 0.5) -1 else 1)

    h1 * w1 + h2 * w2
  }

  def rectangleRotationClever(a: Int, b: Int): Int = {
    val u = (a / math.sqrt(2)).toInt
    val v = (b / math.sqrt(2)).toInt
    (u + 1) * (v + 1) + u * v - (u + v) % 2
  }
}
