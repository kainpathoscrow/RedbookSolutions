// Transplant the implementations of as many combinators as you can from Monad to Applicative, using only map2 and unit, or methods implemented in terms of them

import cats._
import cats.implicits._
import scala.util.Try

// Defs
def traverse[F[_]: Applicative, A, B](as: List[A])(f: A => F[B]): F[List[B]] =
  as.foldRight(Applicative[F].pure(List.empty[B])) { (a: A, acc: F[List[B]]) =>
    val fb: F[B] = f(a)
    Applicative[F].map2(fb, acc)(_ :: _)
  }

// Solution

def sequence[A, F[_]:Applicative](fas: List[F[A]]): F[List[A]] = 
  traverse(fas)((fa: F[A]) => fa)

def replicateM[A, F[_]:Applicative](n: Int, fa: F[A]): F[List[A]] =
  sequence(List.fill(n)(fa))

def product[A,B, F[_]:Applicative](fa: F[A], fb: F[B]): F[(A,B)] =
  Applicative[F].map2(fa, fb)((a, b) => (a, b))

// Tests
val emptyO: List[Option[String]] = Nil
val noneO: List[Option[String]] = List(Some("1"), None)
val someO: List[Option[String]] = List(Some("1"), Some("2"))
val emptyL: List[String] = Nil
val noneL: List[String] = List("1", "abc")
val someL: List[String] = List("1", "2")
def tryToInt(s: String) = Try(s.toInt).toOption

assert(sequence(emptyO) == Some(Nil))
assert(sequence(noneO) == None)
assert(sequence(someO) == Some(List("1", "2")))

assert(replicateM(3, Some(1): Option[Int]) == Some(List(1,1,1)))

assert(product(None, None) == None)
assert(product(Some(1), None) == None)
assert(product(Some(1): Option[Int], Some("2")) == Some((1, "2")))