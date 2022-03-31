package ValidBraces

import scala.annotation.tailrec

object ValidBraces {
  def validBraces(s: String): Boolean = {
    val validBracketsSet = Set("()", "[]", "{}")
    val openBracketsSet = Set("(", "[", "{")
    @tailrec
    def checkValid(s: String, stack: List[String]): Boolean = {
      if (s.isEmpty && stack.isEmpty) return true
      if (s.isEmpty && stack.nonEmpty) return false
      val (ch,rem) = s.splitAt(1)
      if (openBracketsSet.contains(ch))
        checkValid(rem, ch +: stack)
      else if (stack.isEmpty)
        false
      else if (!validBracketsSet.contains(stack.head + ch))
        false
      else checkValid(rem, stack.drop(1))
    }
    checkValid(s, List.empty)
  }

  @tailrec
  def validBracesSimple(s: String): Boolean = s match {
    case str if str.isEmpty => true
    case str if str.length % 2 == 1 => false
    case _ =>
      val replacedBraces = s.replaceAll("(\\(\\)|\\{}|\\[])", "")
      if(s.length == replacedBraces.length)
        false
      else
        validBracesSimple(replacedBraces)
  }
}
