// Reimplement map and map2 in terms of flatMap. 

// Defs
trait RNG {
  def nextInt: (Int, RNG)
}
object OneRNG extends RNG {
  def nextInt: (Int, RNG) = (1, TwoRNG)
}
object TwoRNG extends RNG {
  def nextInt: (Int, RNG) = (2, ThreeRNG)
}
object ThreeRNG extends RNG {
  def nextInt: (Int, RNG) = (3, OneRNG)
}
type Rand[+A] = RNG => (A, RNG)

def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = rng => {
  val (a, rng1) = f(rng)
  (g(a))(rng1)
}

// Solution
def map[A,B](s: Rand[A])(f: A => B): Rand[B] = flatMap(s)(a => (rng => (f(a), rng)))
def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = flatMap(ra)(a => map(rb)(b => f(a, b)))

// Test

assert((map(_.nextInt)(_.toString))(OneRNG) == ("1", TwoRNG))
assert((map2(_.nextInt, _.nextInt)(_ + _))(OneRNG) == (3, ThreeRNG))