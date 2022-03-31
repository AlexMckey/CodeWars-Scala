package RGBToHexConversion

object RGBToHex {
  def rgb(r: Int, g: Int, b: Int): String =
    List(r,g,b)
      .map(x => (if (x < 16) "0" else "") +
        math.min(math.max(x,0),255).toHexString)
      .mkString("")
      .toUpperCase

  def rgbClever(r: Int, g: Int, b: Int): String =
    Seq(r, g, b).map(x => f"${255 min (x max 0)}%02X").mkString
}
