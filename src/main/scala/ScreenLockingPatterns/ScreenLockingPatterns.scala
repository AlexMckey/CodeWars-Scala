package ScreenLockingPatterns

object ScreenLockingPatterns {
  private val Connections = Map(
    'A' -> Array("B_", "CB", "D_", "E_", "F_", "GD", "H_", "IE"),
    'B' -> Array("A_", "C_", "D_", "E_", "F_", "G_", "HE", "I_"),
    'C' -> Array("B_", "AB", "D_", "E_", "F_", "GE", "H_", "IF"),
    'D' -> Array("A_", "B_", "C_", "E_", "FE", "G_", "H_", "I_"),
    'E' -> Array("A_", "B_", "C_", "D_", "F_", "G_", "H_", "I_"),
    'F' -> Array("A_", "B_", "C_", "DE", "E_", "G_", "H_", "I_"),
    'G' -> Array("AD", "B_", "CE", "D_", "E_", "F_", "H_", "IH"),
    'H' -> Array("A_", "BE", "C_", "D_", "E_", "F_", "G_", "I_"),
    'I' -> Array("AE", "B_", "CF", "D_", "E_", "F_", "GH", "H_")
  )
  def countPatternsFrom(from: Char, cnt: Int): Int = {
    def possibleConnections(cur: Char, visited: String): Array[Char] =
      Connections(cur).filter{ s =>
        val Array(to,over) = s.split("")
        !visited.contains(to) && (over == "_" || visited.contains(over))
      }.map(_.head)
    def countrec(pattern: String): Int = {
      var total = 0
      if (pattern.length > cnt) return total
      if (pattern.length == cnt) return total + 1
      val last = pattern.last
      val possible = possibleConnections(last, pattern)
      if (pattern.length + 1 == cnt) possible.length
      possible.map(newPos => countrec(pattern + newPos)).sum
    }
    countrec(from.toString)
  }
}
