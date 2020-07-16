// Implement either flatMap or compose in terms of join and map.

import cats._
import cats.implicits._
import scala.util.Try

// Solution
def flatMap[A,B,F[_]:Monad](v: F[A])(f: A => F[B]): F[B] = {
  val m = implicitly[Monad[F]]
  m.flatten(m.map(v)(f)) // cats' flatten == join
}

// Tests
def tryToInt(s: String) = Try(s.toInt).toOption
def flatMapToInt(s: Option[String]) = flatMap(s)(tryToInt)

assert(flatMapToInt(None) == None)
assert(flatMapToInt(Some("abc")) == None)
assert(flatMapToInt(Some("123")) == Some(123))
