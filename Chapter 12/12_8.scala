// Just like we can take the product of two monoids A and B to give the monoid (A, B), we can take the product of two applicative functors. 
// Implement this function

import cats._
import cats.implicits._
import cats.data._
import cats.data.Validated._

// Solution
def product[F[_], G[_]](f: Applicative[F], g: Applicative[G]): Applicative[({type f[x] = (F[x], G[x])})#f] = new Applicative[({type f[x] = (F[x], G[x])})#f]{
  def pure[A](a: A) = (f.pure(a), g.pure(a))
  
  def ap[A,B](fg: (F[A => B], G[A => B]))(v: (F[A], G[A])): (F[B], G[B]) = {
    (f.ap(fg._1)(v._1), g.ap(fg._2)(v._2))
  }
}

// Tests
val listOptApp = product(Applicative[List], Applicative[Option])
val toStr = (i: Int) => i.toString

assert(listOptApp.pure(1) == (List(1), Some(1)))
assert(listOptApp.ap((Nil, Some(toStr)))(List(1), None) == (Nil, None))
assert(listOptApp.ap((List(toStr), Some(toStr)))(List(1), Some(1)) == (List("1"), Some("1")))