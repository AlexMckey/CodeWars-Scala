import scala.annotation.tailrec

def brackets(n: Int): List[String] = {
  var res = List.empty[String]
  def rec(o: Int, c: Int, a: String = ""): Unit  = {
    if (a.length == 2 * n)
      res = a +: res
    else {
      if (o < c && c > 0)
        rec(o, c - 1, a + ')')
      if (o > 0)
        rec(o - 1, c, a + '(')
    }
  }
  rec(n,n)
  res
}
brackets(3)
val r1 = brackets(4)
r1.size

@tailrec
def genBracket(n: Int, acc: List[String] = List.empty): List[String] =
  if (n == 1) acc
  else genBracket(n-1, acc.flatMap { st => (0 +: st.zipWithIndex
    .filter { ic => ic._1 == '(' }
    .map { _._2+1 })
    .map { new StringBuilder(st).insert(_,"()") }
    .map { _.toString()} }
    .distinct)
genBracket(1, List("()"))
genBracket(2, List("()"))
genBracket(3, List("()"))
val r2 = genBracket(4, List("()"))
val rr2 = genBracket(4, List("()"))
r2.size
r1.iterator.sameElements(r2.sorted)

brackets(10)(8644-1) //

var sb = "()"
def checkValidBrackets(s: String): Boolean = {
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
checkValidBrackets(sb)
sb = "(]"
checkValidBrackets(sb)

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

val s1 = "()()[[[]()]]([()][][()[]])[]()()"
val s2 = "[[]](()()[[[]]][]()()()[()])()]"
val s3 = "[[[((]))[](][)(()())]][[][]()[]]"
val s4 = "(()[([][]())[()][()][][])]([])()"
val s5 = "(()[([][]())[()][()][][]])([])()"
checkValidBrackets(s1) // true
checkValidBrackets(s2) // false
checkValidBrackets(s3) // false
checkValidBrackets(s4) // false
checkValidBrackets(s5) // true

validBracesSimple(s1) // true
validBracesSimple(s2) // false
validBracesSimple(s3) // false
validBracesSimple(s4) // false
validBracesSimple(s5) // true

"((()))[[[]]]"
  .toCharArray
  .combinations(6)
  .flatMap(_.permutations)
  .map(_.mkString(""))
  .filter(checkValidBrackets)
  .toList
  .sorted
  .apply(20-1)
// ([][])

//"((((((()))))))[[[[[[[]]]]]]]"
//  .toCharArray
//  .combinations(14)
//  .flatMap(_.permutations)
//  .map(_.mkString(""))
//  .filter(checkValidBrackets)
//  .toList
//  .sorted
//  .apply(8233-1)
// (([]())([]))() очень долго - 9 минут
