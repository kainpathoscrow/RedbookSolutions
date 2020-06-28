/* Implement sequence for combining a List of transitions into a single transition. Use it to reimplement the ints function you wrote before. 
For the latter, you can use the standard library function List.fill(n)(x) to make a list with x repeated n times. */

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
def map[A,B](s: Rand[A])(f: A => B): Rand[B] = rng => {
  val (a, rng2) = s(rng)
  (f(a), rng2)
}
def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = rng => {
  val (a, rngA) = ra(rng)
  (map(rb)(b => f(a, b)))(rngA)
}

// Solution
def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
  fs.foldRight((rng: RNG) => (Nil: List[A], rng))((cur, acc) => map2(cur, acc)(_ :: _))

def ints(n: Int): Rand[List[Int]] = {
  val randList = List.fill(n)(int)
  sequence(randList)
}

// Manual test (usage example)
val (rl, _) = (ints(15))(SimpleRNG(123))
println(rl)
