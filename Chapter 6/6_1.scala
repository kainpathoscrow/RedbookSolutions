/* Write a function that uses RNG.nextInt to generate a random integer between 0 and Int.maxValue (inclusive). 
Make sure to handle the corner case when nextInt returns Int.MinValue, which doesn’t have a non-negative counterpart. */

// Defs
trait RNG {
  def nextInt: (Int, RNG)
}
case class SimpleRNG(seed: Long) extends RNG {
  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }
}

// Solution

def nonNegativeInt(rng: RNG): (Int, RNG) = {
  val (i, nextRng) = rng.nextInt
  val positiveInt = if (i < 0) (-1 * (i + 1)) else i
  (positiveInt, nextRng)
}

// Test
val rngs = (1 to 1000000).foldRight((Nil: Seq[Int], SimpleRNG(123): RNG)){
  case (_, (accSeq, accRng)) => {
    val (i, nextRng) = nonNegativeInt(accRng)
    (i +: accSeq, nextRng)
  }
}._1
assert(rngs.forall(_ > 0))