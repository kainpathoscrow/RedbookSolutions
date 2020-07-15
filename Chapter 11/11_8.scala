// Implement flatMap in terms of compose.

import cats._
import cats.implicits._
import scala.util.Try

// Defs
def compose[A,B,C,F[_]:Monad](f: A => F[B], g: B => F[C]): A => F[C] = {
  val m = implicitly[Monad[F]]
  a => m.flatMap(f(a))(g)
}

// Solution
def flatMap[A,B,F[_]:Monad](v: F[A])(f: A => F[B]): F[B] = {
  val m = implicitly[Monad[F]]
  val getV: Unit => F[A] = _ => v
  val composed = compose(getV, f)
  composed(())
}

// Tests
def tryToInt(s: String) = Try(s.toInt).toOption
def flatMapToInt(s: Option[String]) = flatMap(s)(tryToInt)

assert(flatMapToInt(None) == None)
assert(flatMapToInt(Some("abc")) == None)
assert(flatMapToInt(Some("123")) == Some(123))
