// Implement join in terms of flatMap. 

import cats._
import cats.implicits._

// Solution
def join[A,F[_]:Monad](mma: F[F[A]]): F[A] = {
  val m = implicitly[Monad[F]]
  m.flatMap(mma)(innerMa: F[A] => innerMa)
}

assert(join(Some(None: Option[String])) == None)
assert(join(None: Option[Option[String]]) == None)
assert(join(Some(Some(123): Option[Int])) == Some(123))