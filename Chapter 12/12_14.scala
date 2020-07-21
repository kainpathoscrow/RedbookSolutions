// Implement map in terms of traverse as a method on Traverse[F].

import cats._
import cats.implicits._

// Solution
def map[A,B,F[_]:Traverse](fa: F[A])(f: A => B): F[B] = {
  val fT = implicitly[Traverse[F]]
  val idA = implicitly[Applicative[Id]]
  fT.traverse[Id,A,B](fa)(a => f(a))(idA)
}

// Tests
val none = None: Option[Int]
val some = Some(1): Option[Int]
assert(map(none)(_.toString) == None)
assert(map(some)(_.toString) == Some("1"))
