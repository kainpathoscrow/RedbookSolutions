//Implement map3 and map4 using only unit, apply, and the curried method available on functions

import cats._
import cats.implicits._

// Solution

def map3[A,B,C,D,F[_]:Applicative](fa: F[A], fb: F[B], fc: F[C])(f: (A,B,C) => D): F[D] = {
  val af = Applicative[F]
  af.ap(af.ap(af.ap(af.pure(f.curried))(fa))(fb))(fc)
}

def map3[A,B,C,D,E,F[_]:Applicative](fa: F[A], fb: F[B], fc: F[C], fd: F[D])(f: (A,B,C,D) => E): F[E] = {
  val af = Applicative[F]
  af.ap(af.ap(af.ap(af.ap(af.pure(f.curried))(fa))(fb))(fc))(fd)
}