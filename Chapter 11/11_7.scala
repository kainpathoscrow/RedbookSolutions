// Implement the Kleisli composition function compose.

import cats._
import cats.implicits._

// Solution
def compose[A,B,C,F[_]:Monad](f: A => F[B], g: B => F[C]): A => F[C] = {
  val m = implicitly[Monad[F]]
  a => m.flatMap(f(a))(g)
}

// Tests
def divideBy2(i: Int): Option[Int] = if (i % 2 == 0) Some(i/2) else None
def toStringIfDivisibleBy3(i: Int): Option[String] = if (i % 3 == 0) Some(i.toString) else None
val composition = compose(divideBy2, toStringIfDivisibleBy3)

assert(composition(1) == None)
assert(composition(2) == None)
assert(composition(3) == None)
assert(composition(12) == Some("6"))