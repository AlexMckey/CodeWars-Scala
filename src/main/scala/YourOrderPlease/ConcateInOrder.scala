package YourOrderPlease

object ConcateInOrder {
  def order(str: String): String =
    str
      .split(' ')
      .sortBy(_.find(_.isDigit))
      .mkString(" ")
}
