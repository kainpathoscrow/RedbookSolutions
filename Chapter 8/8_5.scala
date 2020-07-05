// Let’s see what else we can implement using this representation of Gen. Try implementing unit, boolean, and listOfN.

// Defs RNG
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

// Defs Gen
case class Gen[A](sample: State[RNG,A])
def choose(start: Int, stopExclusive: Int): Gen[Int] = {
  val diff = stopExclusive - start
  val sample = nonNegativeLessThan(diff).map(i => i + start)
  Gen(sample)
}

// Solution
def unit[A](a: => A): Gen[A] = Gen(State.unit(a))
def boolean: Gen[Boolean] = Gen(nonNegativeLessThan(2).map(_ == 0))
def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] = Gen(State.sequence(List.fill(n)(g.sample)))

// Tests
assert(unit(10).sample.run(SimpleRNG(123))._1 == 10)

val booleans = (State.sequence(List.fill(1000)(boolean.sample))).run(SimpleRNG(123))._1
assert(booleans.exists(b => b))
assert(booleans.exists(b => !b))
assert(booleans.filter(b => b).length <= 525 && booleans.filter(b => b).length >= 475)

val betweenMinus5And5List = listOfN(1000, choose(-5, 6)).sample.run(SimpleRNG(123))._1
assert(betweenMinus5And5List.forall(i => i >= -5 && i <= 5))
assert(betweenMinus5And5List.exists(i => i == -5))
assert(betweenMinus5And5List.exists(i => i == 5))
