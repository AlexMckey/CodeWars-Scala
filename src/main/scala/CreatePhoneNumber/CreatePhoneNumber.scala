package CreatePhoneNumber

object CreatePhoneNumber {
  def createPhoneNumber(numbers: Seq[Int]): String = numbers match {
    case a::b::c::d::e::f::t => s"($a$b$c) $d$e$f-${t.mkString("")}"
    case _ => ""
  }
  //s"(${numbers.take(3).mkString}) ${numbers.slice(3,6).mkString}-${numbers.drop(6).mkString}"
}
