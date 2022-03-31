package MorseCode

object DecodeRealMorseCode {
  //import MorseCodes.MORSE_CODE

  import scala.annotation.tailrec

  val morseCode = Map(
    ".-" -> "A",
    "-..." -> "B",
    "-.-." -> "C",
    "-.." -> "D",
    "." -> "E",
    "..-." -> "F",
    "--." -> "G",
    "...." -> "H",
    ".." -> "I",
    ".---" -> "J",
    "-.-" -> "K",
    ".-.." -> "L",
    "--" -> "M",
    "-." -> "N",
    "---" -> "O",
    ".--." -> "P",
    "--.-" -> "Q",
    ".-." -> "R",
    "..." -> "S",
    "-" -> "T",
    "..-" -> "U",
    "...-" -> "V",
    ".--" -> "W",
    "-..-" -> "X",
    "-.--" -> "Y",
    "--.." -> "Z",
    ".----" -> "1",
    "..---" -> "2",
    "...--" -> "3",
    "....-" -> "4",
    "....." -> "5",
    "-...." -> "6",
    "--..." -> "7",
    "---.." -> "8",
    "----." -> "9",
    "-----" -> "0",
    "-.-.--" -> "!",
    ".-.-.-" -> ".",
    "...---..." -> "SOS")

  @tailrec
  private def rle(rem: Seq[Char], prev: Char, cnt: Int = 1, acc: List[(Char,Int)] = List.empty): List[(Char,Int)] = {
    if (rem.isEmpty) return acc :+ (prev -> cnt)
    val cur = rem.head
    if (prev == cur) rle(rem.tail, cur, cnt + 1, acc)
    else rle(rem.tail, cur, 1, acc :+ (prev -> cnt))
  }

  private def signalToMorseCode(signal: (Char,Int)): String = {
    signal match {
      case ('1',1) => "."
      case ('1',_) => "-"
      case (_,1) => ""
      case (_,2) => " "
      case (_,3) => " "
      case _ => "   "
    }
  }
  def decodeBitsAdvanced(bits: String): String = {
    val pbits = bits.dropWhile(_ == '0').reverse.dropWhile(_ == '0').reverse
    if (pbits.isEmpty) return ""
    val signalPairs = rle(pbits.tail, pbits.head)
    val avgRate = signalPairs.map(_._2).sum.toDouble/signalPairs.length
    val avgSignalPairs = signalPairs.map{ case(fst,snd) => fst -> (snd/avgRate).ceil.toInt }
    avgSignalPairs.map(signalToMorseCode).mkString("")
  }

  def decodeMorse(code: String): String = {
    if (code.isEmpty || code.isBlank) return ""
    code.trim
      .split(" {3}")
      .map(_.split(" ")
        .map { code => morseCode.getOrElse(code,"")}
        .mkString(""))
      .mkString(" ")
  }
}
