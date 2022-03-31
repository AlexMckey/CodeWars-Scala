val a = Seq(1, 2, 3, 4, 5, 6, 7, 8, 9, 0)
val k = a.mkString("")
val (f,q) = k.splitAt(3)
val (s,t) = q.splitAt(3)
s"(${f.mkString("")}) ${s.mkString("")}-${t.mkString("")}"
s"($f) $s-$t"
a match {
  case a::b::c::d::e::f::g::i::j::k::_ => s"($a$b$c) $d$e$f-$g$i$j$k"
  case _ => ""
}