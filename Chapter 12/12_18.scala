// Use applicative functor products to write the fusion of two traversals.

import cats._
import cats.data._
import cats.implicits._

// Defs
def product[F[_], G[_]](f: Applicative[F], g: Applicative[G]): Applicative[({type f[x] = (F[x], G[x])})#f] = new Applicative[({type f[x] = (F[x], G[x])})#f]{
  def pure[A](a: A) = (f.pure(a), g.pure(a))
  
  def ap[A,B](fg: (F[A => B], G[A => B]))(v: (F[A], G[A])): (F[B], G[B]) = {
    (f.ap(fg._1)(v._1), g.ap(fg._2)(v._2))
  }
}

// Solution
def fuse[G[_]:Applicative,H[_]:Applicative,A,B,F[_]:Traverse](fa: F[A])(g: A => G[B], h: A => H[B]): (G[F[B]], H[F[B]]) = {
  val appProduct = product(implicitly[Applicative[G]], implicitly[Applicative[H]])
  Traverse[F].traverse[({type f[x] = (G[x], H[x])})#f, A, B](fa)(a => (g(a), h(a)))(appProduct)
}

val iToNone = (i: Int) => None: Option[Int]
val iToSome = (i: Int) => Some(i): Option[Int]
val iToRight = (i: Int) => Right(i): Either[Unit, Int]
val iToList = (i: Int) => List(i)

assert(fuse(List(1,2,3))(iToSome, iToNone) == (Some(List(1,2,3)), None))
assert(fuse(List(1,2,3))(iToRight, iToList) == (Right(List(1,2,3)), List(List(1,2,3))))