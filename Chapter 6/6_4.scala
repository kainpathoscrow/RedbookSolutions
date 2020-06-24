// Write a function to generate a list of random integers

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

def ints(count: Int)(rng: RNG): (List[Int], RNG) = (1 to count).foldRight((Nil: List[Int], rng)){
  case (_, (accSeq, accRng)) => {
    val (i, nextRng) = accRng.nextInt
    (i +: accSeq, nextRng)
  }
}