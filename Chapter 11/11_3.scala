/* The sequence and traverse combinators should be pretty familiar to you by now, 
and your implementations of them from various prior chapters are probably all very similar.
Implement them once and for all on Monad[F]. */

import cats._
import cats.implicits._
import scala.util.Try

// Solution
def sequence[A, M[_]](mas: List[M[A]])(implicit m: Monad[M]): M[List[A]] =
  mas.foldLeft(m.pure(Nil: List[A]))((acc, cur) => m.flatMap(acc)(accV => m.map(cur)(curV => accV :+ curV)))

def traverse[A,B, M[_]](as: List[A])(f: A => M[B])(implicit m: Monad[M]): M[List[B]] =
  sequence(as.map(f))


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

assert(traverse(emptyL)(tryToInt) == Some(Nil))
assert(traverse(noneL)(tryToInt) == None)
assert(traverse(someL)(tryToInt) == Some(List(1, 2)))
