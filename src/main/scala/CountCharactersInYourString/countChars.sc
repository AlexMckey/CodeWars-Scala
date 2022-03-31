val s = "aba"

s.groupBy(identity).map{ case (a,b) => a -> b.length }
"".groupBy(identity).map{ case (a,b) => a -> b.length }