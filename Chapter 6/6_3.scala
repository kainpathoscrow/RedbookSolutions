// Write functions to generate an (Int,  Double) pair, a (Double,  Int) pair, and a (Double, Double, Double) 3-tuple.

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
def nonNegativeInt(rng: RNG): (Int, RNG) = {
  val (i, nextRng) = rng.nextInt
  val positiveInt = if (i < 0) (-1 * (i + 1)) else i
  (positiveInt, nextRng)
}
def double(rng: RNG): (Double, RNG) = {
  val (i, nextRng) = nonNegativeInt(rng)
  val dbl = i.toDouble / (Int.MaxValue.toDouble + 1)
  (dbl, nextRng)
}

// Solution

def intDouble(rng: RNG): ((Int,Double), RNG) = {
  val (i, rng1) = rng.nextInt
  val (d, rng2) = double(rng1)
  ((i, d), rng2)
}
def doubleInt(rng: RNG): ((Double,Int), RNG) = {
  val (d, rng1) = double(rng)
  val (i, rng2) = rng1.nextInt
  ((d, i), rng2)
}
def double3(rng: RNG): ((Double,Double,Double), RNG) = {
  val (d1, rng1) = double(rng)
  val (d2, rng2) = double(rng1)
  val (d3, rng3) = double(rng2)  
  ((d1, d2, d3), rng3)
}
