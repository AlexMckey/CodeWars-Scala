package ConvertStringToCamelCase

object ConvToCamelCase {
  def toCamelCase(str: String): String = {
    val s = str.split("-_".toCharArray)
    s.head + s.tail.map(_.capitalize).mkString("")
    //str.split("[_-]").reduce((a, b) => a + b.capitalize)
  }
}
