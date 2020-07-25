// Free is a monad for any choice of F. Implement map and flatMap methods on the Free trait, and give the Monad instance for Free[F,_]

import cats._

// Solution
sealed trait Free[F[_],A] {
  def map[B](f: A => B): Free[F, B] = flatMap(a => Return(f(a)))
  def flatMap[B](f: A => Free[F, B]): Free[F, B] = FlatMap(this, f)
}
case class Return[F[_],A](a: A) extends Free[F,A]
case class Suspend[F[_],A](s: F[A]) extends Free[F,A]
case class FlatMap[F[_],A,B](s: Free[F,A], f: A => Free[F,B]) extends Free[F,B]


def freeMonad[F[_]]: Monad[({type f[a] = Free[F,a]})#f] = new Monad[({type f[a] = Free[F,a]})#f] {
  def pure[A](a: A) = Return(a)
  def flatMap[A,B](fa: Free[F, A])(f: A => Free[F, B]) = fa.flatMap(f)
  
  def tailRecM[A, B](a: A)(f: A => Playground.Free[F,Either[A,B]]): Playground.Free[F,B] = ??? // cats-required
}