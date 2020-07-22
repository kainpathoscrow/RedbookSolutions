// Use mapAccum to give a default implementation of foldLeft for the Traverse trait.

import cats._
import cats.data._
import cats.implicits._

// Defs
def traverseS[S,A,B,F[_]:Traverse](fa: F[A])(f: A => State[S, B]): State[S, F[B]] =
  Traverse[F].traverse[({type f[x] = State[S,x]})#f,A,B](fa)(f)(implicitly[Monad[({type f[x] = State[S,x]})#f]])

def mapAccum[S,A,B,F[_]:Traverse](fa: F[A], s: S)(f: (A, S) => (B, S)): (F[B], S) = {
  traverseS(fa)((a: A) => (for {
    s1 <- State.get[S]
    (b, s2) = f(a, s1)
    _ <- State.set(s2)
  } yield b)).run(s).value.swap
}

// Solution
def foldLeft[A,B,F[_]:Traverse](fa: F[A])(z: B)(f: (B, A) => B): B =
  mapAccum(fa,z)((a, b) => ((), f(b,a)))._2

// Test
assert(foldLeft(Nil)("a")((a, b) => a+b) == "a")
assert(foldLeft(List(1,2,3))(0)((a, b) => a+b) == 6)
assert(foldLeft(List(1,2,3))(0)((a, b) => a-b) == -6)