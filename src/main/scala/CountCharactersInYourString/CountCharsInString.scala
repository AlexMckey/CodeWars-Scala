package CountCharactersInYourString

object CountCharsInString {
  def count(string: String): Map[Char,Int] = {
    //string.groupBy(identity).view.mapValues(_.size).toMap
    string.groupMapReduce(identity)(_ => 1)(_ + _)
  }
}
