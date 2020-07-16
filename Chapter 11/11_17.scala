// Implement map and flatMap as methods on this class, and give an implementation for Monad[Id].

import cats._
import scala.annotation.tailrec

// Solution
def mapId[A, B](id: Id[A])(f: A => B): Id[B] = f(id)
def flatMapId[A, B](id: Id[A])(f: A => Id[B]): Id[B] = f(id)

val idMonad = new Monad[Id] {
  def pure[A](a: A) = a
  def flatMap[A, B](id: Id[A])(f: A => Id[B]) = flatMapId(id)(f)
  
  @tailrec
  def tailRecM[A, B](a: A)(f: A => Id[Either[A, B]]): Id[B] = f(a) match {
    case Left(nextA) => tailRecM(nextA)(f)
    case Right(b) => b
  }
}

// Tests
assert(idMonad.pure(1) == 1)
assert(idMonad.flatMap(10)(_ + 15) == 25)