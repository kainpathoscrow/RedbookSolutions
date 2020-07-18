// Write a monad instance for Either.

import cats._
import cats.Monad
import scala.annotation.tailrec

// Solution
def eitherMonad[E]: Monad[({type f[x] = Either[E, x]})#f] = new Monad[({type f[x] = Either[E, x]})#f]{
  def pure[A](a: A) = Right(a)
  def flatMap[A,B](e: Either[E, A])(f: A => Either[E, B]) = e match {
    case Left(l) => Left(l)
    case Right(r) => f(r)
  }
  
  @tailrec
  def tailRecM[A, B](a: A)(f: A => Either[E, Either[A, B]]): Either[E, B] = f(a) match {
      case Left(l) => Left(l)
      case Right(Left(l)) => tailRecM(l)(f)
      case Right(Right(r)) => Right(r)
    }
}