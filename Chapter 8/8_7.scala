// Implement union, for combining two generators of the same type into one, by pulling values from each generator with equal likelihood.


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
case class State[S,+A](run: S => (A,S)) {
  def flatMap[B](f: A => State[S, B]): State[S, B] =
    State(s => {
      val (a, s1) = run(s)
      f(a).run(s1)
    })
  
  def map[B](f: A => B): State[S, B] = flatMap(a => State.unit(f(a)))
  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] = flatMap(a => sb.map(b => f(a, b)))
}
object State {
  def unit[S, A](a: A): State[S, A] = State(s => (a, s))
  
  def sequence[S, A](states: List[State[S, A]]): State[S, List[A]] = State(st => 
    states.reverse.foldRight((Nil: List[A], st)) {
      case (cur, (accList, accS)) => {
        val (nextA, nextS) = cur.run(accS)
        (nextA +: accList, nextS)
      }
    })    
}
val int: State[RNG,Int] = State(_.nextInt)
def nonNegativeInt: State[RNG,Int] = int.map(i => if (i < 0) (-1 * (i + 1)) else i)
def nonNegativeLessThan(n: Int): State[RNG,Int] = nonNegativeInt.flatMap(i => {
  val mod = i % n
  if (i + (n-1) - mod >= 0)
    State(rng => (mod, rng))
  else nonNegativeLessThan(n)
})

def choose(start: Int, stopExclusive: Int): Gen[Int] = {
  val diff = stopExclusive - start
  val sample = nonNegativeLessThan(diff).map(i => i + start)
  Gen(sample)
}

case class Gen[A](sample: State[RNG,A]) {
  def flatMap[B](f: A => Gen[B]): Gen[B] = Gen(sample.flatMap(f(_).sample))
  def listOfN(size: Int): Gen[List[A]] = Gen(State.sequence(List.fill(size)(sample)))
  def listOfN(size: Gen[Int]): Gen[List[A]] = size.flatMap(s => listOfN(s))
}

// Solution
def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] = choose(0, 2).flatMap(i => if (i == 0) g1 else g2)

// Tests
val g1 = Gen(State(rng => (true, rng)))
val g2 = Gen(State(rng => (false, rng)))
val booleans = (State.sequence(List.fill(1000)(union(g1, g2).sample))).run(SimpleRNG(123))._1
assert(booleans.exists(b => b))
assert(booleans.exists(b => !b))
assert(booleans.filter(b => b).length <= 525 && booleans.filter(b => b).length >= 475)