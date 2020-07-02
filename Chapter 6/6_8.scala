// Implement flatMap, and then use it to implement nonNegativeLessThan.


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

def map[A,B](s: Rand[A])(f: A => B): Rand[B] = rng => {
  val (a, rng2) = s(rng)
  (f(a), rng2)
}
def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = rng => {
  val (a, rngA) = ra(rng)
  (map(rb)(b => f(a, b)))(rngA)
}
def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = rng => 
  fs.reverse.foldRight((Nil: List[A], rng)){
    case (cur, (accList, accRng)) => {
      val (d, nextRng) = cur(accRng)
      (d +: accList, nextRng)
    }
}

val int: Rand[Int] = _.nextInt
def nonNegativeInt = map(int)(i => if (i < 0) (-1 * (i + 1)) else i)

// Solution
def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = rng => {
  val (a, rng1) = f(rng)
  (g(a))(rng1)
}

def nonNegativeLessThan(n: Int): Rand[Int] = flatMap(nonNegativeInt)(i => {
  val mod = i % n
  if (i + (n-1) - mod >= 0)
    rng => (mod, rng)
  else nonNegativeLessThan(n)
})

// Test 
val nonNegativesLessThan5 = sequence(List.fill(10000)(nonNegativeLessThan(5)))(SimpleRNG(123))._1
assert(nonNegativesLessThan5.forall(i => i >= 0 && i < 5))