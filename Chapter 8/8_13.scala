// Define listOf1 for generating nonempty lists, and then update your specification of max to use this generator.

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
case class Gen[+A](sample: State[RNG,A]) {
  def flatMap[B](f: A => Gen[B]): Gen[B] = Gen(sample.flatMap(f(_).sample))
  def map[B](f: A => B): Gen[B] = Gen(sample.map(f))
  def listOfN(size: Int): Gen[List[A]] = Gen(State.sequence(List.fill(size)(sample)))
  def listOfN(size: Gen[Int]): Gen[List[A]] = size.flatMap(s => listOfN(s))
  def unsized: SGen[A] = SGen(i => this)
}
case class SGen[+A](forSize: Int => Gen[A]) {
  def flatMap[B](f: A => SGen[B]): SGen[B] = SGen(i => {
    forSize(i).flatMap(a => f(a).forSize(i))
  })  
  def listOfN(size: Int): SGen[List[A]] = SGen(i => {
    forSize(i).listOfN(size)
  })    
  def listOfN(size: Gen[Int]): SGen[List[A]] = SGen(i => {
    forSize(i).listOfN(size)
  })  
  def toGen(n: Int): Gen[A] = forSize(n)
}

type FailedCase = String
type SuccessCount = Int
type MaxSize = Int
type TestCases = Int
sealed trait Result {
  def isFalsified: Boolean
}
case object Passed extends Result {
  def isFalsified = false
}
case class Falsified(failure: FailedCase, successes: SuccessCount) extends Result {
  def isFalsified = true
}
case class Prop(run: (MaxSize,TestCases,RNG) => Result) {
  def &&(p: Prop): Prop = Prop((maxSize,tcs,rng) => 
    run(maxSize, tcs, rng) match {
      case Passed => p.run(maxSize, tcs, rng)
      case f @ Falsified(_, _) => f
    })  
  def ||(p: Prop): Prop = Prop ((maxSize,tc,rng) => 
    run(maxSize, tc, rng) match {
      case pass @ Passed => pass
      case f @ Falsified(_, _) => p.run(maxSize, tc, rng)
    })
}
def forAll[A](g: SGen[A])(f: A => Boolean): Prop =
  forAll(s => g.forSize(s))(f)
def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop = Prop {
  (max,n,rng) =>
    val casesPerSize = (n + (max - 1)) / max
    val props: Stream[Prop] =
      Stream.from(0).take((n min max) + 1).map(i => forAll(g)(f))
    val prop: Prop =
      props.map(p => Prop { (max, _, rng) =>
        p.run(max, casesPerSize, rng)
      }).toList.reduce(_ && _)
  prop.run(max,n,rng)
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

// Solution
import scala.math.{abs, max}
def listOf1[A](g: Gen[A]): SGen[List[A]] = SGen(size => g.listOfN(abs(size) max 1))
val smallInt = choose(-10,10)
val maxProp = forAll(listOf1(smallInt)) { ns =>
  val max = ns.max
  !ns.exists(_ > max)
}