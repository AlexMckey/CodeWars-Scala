def isPrime(self: Int): Boolean = {
  if (self <= 3) return true
  else if (self % 2 == 0) return false
  for (i <- 2 to math.sqrt(self).toInt + 1) {
    if (self % i == 0) return false
  }
  true
}
val n = 1 to 1000
n.filter(isPrime)

def nextPrime(n: Int): Int = {
  LazyList.from(if (n % 2 == 0) n+1 else n+2,2).dropWhile(!isPrime(_)).head
}

def gapPrime(g: Int, f: Int, e: Int): String = {
  val p = (f to e).toList
    .dropWhile(x =>
      !isPrime(x) ||
        (isPrime(x) && nextPrime(x) - x != g))
    .headOption
  p match {
    case Some(v) => (v,v+g).toString()
    case _ => ""
  }
}
gapPrime(2,3,50)
gapPrime(2,100,100)
gapPrime(2,100,110)
gapPrime(3,100,1500)
gapPrime(4,130,200)
gapPrime(4,100,110)
gapPrime(6,100,110)
nextPrime(110)
nextPrime(3)

gapPrime(8, 100, 400)
gapPrime(2,2300,2400)

(330 to 370).filter(isPrime).toList
//  .sliding(2).toList
//  .map{a => a(1) - a(0)}

BigInt(330).to(370).filter(_.isProbablePrime(4))
  .sliding(2)
  .find{
    case IndexedSeq(a,b) => b - a == 8
  }.map(l => (l(0),l(1)).toString)
  .getOrElse("")