val s1 = "the-stealth-warrior"
val s2 = "The_Stealth_Warrior"
var s = s1.split("-_".toCharArray).map(_.capitalize).mkString("")
s1.head +: s.tail
s = s2.split("-_".toCharArray).map(_.capitalize).mkString("")
s2.head +: s.tail

val t = s1.split("-_".toCharArray)
t.head + t.tail.map(_.capitalize).mkString("")

val r = s2.split("-_".toCharArray)
r.head + r.tail.map(_.capitalize).mkString("")