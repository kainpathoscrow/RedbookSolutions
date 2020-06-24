// Use map to reimplement double in a more elegant way.

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

type Rand[+A] = RNG => (A, RNG)
val int: Rand[Int] = _.nextInt
def unit[A](a: A): Rand[A] = rng => (a, rng)
def map[A,B](s: Rand[A])(f: A => B): Rand[B] = rng => {
  val (a, rng2) = s(rng)
  (f(a), rng2)
}

// Solution
def double: Rand[Double] = {
  val positiveInt: Rand[Int] = map(_.nextInt)(i => if (i < 0) (-1 * (i + 1)) else i)
  map(positiveInt)(i => i.toDouble / (Int.MaxValue.toDouble + 1))
}

// Test
val rngs = (1 to 1000000).foldRight((Nil: Seq[Double], SimpleRNG(123): RNG)){
  case (_, (accSeq, accRng)) => {
    val (d, nextRng) = double(accRng)
    (d +: accSeq, nextRng)
  }
}._1
assert(rngs.forall(d => d >= 0 && d < 1))