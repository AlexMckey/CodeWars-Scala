import java.util.Random

class ReplicatedRandom extends Random {
  def replicateState(nextN: Int, n: Int, nextM: Int, m: Int): Boolean = { // Constants copied from java.util.Random
    val multiplier = 0x5DEECE66DL
    val addend = 0xBL
    val mask = (1L << 48) - 1
    val upperMOf48Mask = ((1L << m) - 1) << (48 - m)
    // next(x) is generated by taking the upper x bits of 48 bits of (oldSeed * multiplier + addend) mod (mask + 1)
    // So now we have the upper n and m bits of two consecutive calls of next(n) and next(m)
    val oldSeedUpperN = (nextN.toLong << (48 - n)) & mask
    val newSeedUpperM = (nextM.toLong << (48 - m)) & mask
    // Bruteforce the lower (48 - n) bits of the oldSeed that was truncated.
    // Calculate the next seed for each guess of oldSeed and check if it has the same top m bits as our newSeed.
    // If it does then the guess is right and we can add that to our candidate seeds.
    val possibleSeeds = scala.collection.mutable.ListBuffer[Long]()
    for (oldSeed <- oldSeedUpperN to (oldSeedUpperN | ((1L << (48 - n)) - 1))) {
      val newSeed = (oldSeed * multiplier + addend) & mask
      if ((newSeed & upperMOf48Mask) == newSeedUpperM) possibleSeeds.append(newSeed)
    }
    if (possibleSeeds.size == 1) { // If there's only one candidate seed, then we found it!
      setSeed(possibleSeeds.head ^ multiplier) // setSeed(x) sets seed to `(x ^ multiplier) & mask`, so we need another `^ multiplier` to cancel it out

      return true
    }
    if (possibleSeeds.nonEmpty) System.out.println("Didn't find a unique seed. Possible seeds were: " + possibleSeeds)
    else System.out.println("Failed to find seed!")
    false
  }

  // Replicate the state of a Random using a single value from its nextDouble// Replicate the state of a Random using a single value from its nextDouble

  def replicateState(nextDouble: Double): Boolean = { // nextDouble() is generated from ((next(26) << 27) + next(27)) / (1L << 53)
    // Inverting those operations will get us the values of next(26) and next(27)
    val numerator = (nextDouble * (1L << 53)).toLong
    val first26 = (numerator >>> 27).toInt
    val last27 = (numerator & ((1L << 27) - 1)).toInt
    replicateState(first26, 26, last27, 27)
  }
}

val rr = new ReplicatedRandom()
rr.replicateState(Math.random)
System.out.println(Math.random == rr.nextDouble) // True
System.out.println(Math.random == rr.nextDouble)
System.out.println(Math.random == rr.nextDouble)