// Implement a generic interpreter for Free[F,A], given a Monad[F]. 
// You can pattern your implementation after the Async interpreter given previously, 
// including use of a tail-recursive step function.

import cats._

// Defs
sealed trait Free[F[_],A] {
  def map[B](f: A => B): Free[F, B] = flatMap(a => Return(f(a)))
  def flatMap[B](f: A => Free[F, B]): Free[F, B] = FlatMap(this, f)
}
case class Return[F[_],A](a: A) extends Free[F,A]
case class Suspend[F[_],A](s: F[A]) extends Free[F,A]
case class FlatMap[F[_],A,B](s: Free[F,A], f: A => Free[F,B]) extends Free[F,B]

@annotation.tailrec
def step[F[_], A](free: Free[F,A]): Free[F,A] = free match {
  case FlatMap(FlatMap(x,f), g) => step(x flatMap (a => f(a) flatMap g))
  case FlatMap(Return(x), f) => step(f(x))
  case _ => free
}

// Solution
def run[F[_]: Monad,A](a: Free[F,A]): F[A] = step(a) match {
  case Return(a) => Monad[F].pure(a)
  case Suspend(r) => r
  case FlatMap(x, f) => x match {
    case Suspend(r) => Monad[F].flatMap(r)(a => run(f(a)))
    case _ => sys.error("Impossible; `step` eliminates these cases")
  }
}