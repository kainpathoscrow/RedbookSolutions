//  Implement the function filterM. 
// It’s a bit like filter, except that instead of a function from A => Boolean, we have an A  =>  F[Boolean]

import cats._
import cats.implicits._

// Defs
def filterM[A, F[_]: Monad](ms: List[A])(f: A => F[Boolean]): F[List[A]] = {
  val m = implicitly[Monad[F]]
  ms match {
    case head :: tail => {
      val boolM = f(head)
      m.flatMap(boolM)(bool => if (bool) m.map(filterM(tail)(f))(t => head :: t) else filterM(tail)(f))
    }
    case Nil => m.pure(Nil)
  }
}

def isLessThan3Some(a: Int): Option[Boolean] = if (a < 3) Some(true) else Some(false)
def isLessThan3None(a: Int): Option[Boolean] = if (a < 3) Some(true) else None

assert(filterM(List(1,2,3))(isLessThan3Some) == Some(List(1, 2)))
assert(filterM(List(1,2,3))(isLessThan3None) == None)