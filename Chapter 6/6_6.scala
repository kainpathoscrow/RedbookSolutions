/* Write the implementation of map2 based on the following signature. 
This function takes two actions, ra and rb, and a function f for combining their results, and returns a new action that combines them */

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
def map[A,B](s: Rand[A])(f: A => B): Rand[B] = rng => {
  val (a, rng2) = s(rng)
  (f(a), rng2)
}

// Solution

def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = rng => {
  val (a, rngA) = ra(rng)
  (map(rb)(b => f(a, b)))(rngA)
}

assert((map2(_.nextInt, _.nextInt)(_ + _))(OneRNG) == (3, ThreeRNG))