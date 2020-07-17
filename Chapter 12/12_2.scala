// Show that this formulation is equivalent in expressiveness by defining map2 and map in terms of unit and apply. 
// Also establish that apply can be implemented in terms of map2 and unit.

import cats._
import cats.implicits._

// Solution
def map[A,B,F[_]:Applicative](fa: F[A])(f: A => B): F[B] = {
  val af = Applicative[F]
  af.ap(af.pure(f))(fa)
}


def map2[A,B,C,F[_]:Applicative](fa: F[A], fb: F[B])(f: (A,B) => C): F[C] = {
  val af = Applicative[F]
  val pureCurried = af.pure((a: A) => ((b: B) => f(a, b)))
  af.ap(af.ap(pureCurried)(fa))(fb)
}


def apply[A,B,F[_]:Applicative](fab: F[A => B])(fa: F[A]): F[B] = {
  val af = Applicative[F]
  map2(fab, fa)((ab, a) => ab(a))
}

// Tests  
val noneInt: Option[Int] = None
val some1: Option[Int] = Some(1)
val some2: Option[Int] = Some(2)
assert(map(noneInt)(_.toString) == None)
assert(map(some1)(_.toString) == Some("1"))

assert(map2(noneInt, noneInt)(_ + _) == None)
assert(map2(noneInt, some1)(_ + _) == None)
assert(map2(some1, noneInt)(_ + _) == None)
assert(map2(some1, some2)(_ + _) == Some(3))

def pureNone = None: Option[Int => String]
def pureToString = Some((i: Int) => i.toString): Option[Int => String]
assert(apply(pureNone)(noneInt) == None)
assert(apply(pureNone)(some1) == None)
assert(apply(pureToString)(noneInt) == None)
assert(apply(pureToString)(some1) == Some("1"))