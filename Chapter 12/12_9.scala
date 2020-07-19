// If F[_] and G[_] are applicative functors, then so is F[G[_]]. Implement this function

import cats._
import cats.implicits._
import cats.data._
import cats.data.Validated._

// Solution
def compose[F[_], G[_]](f: Applicative[F], g: Applicative[G]): Applicative[({type f[x] = F[G[x]]})#f] = new Applicative[({type f[x] = F[G[x]]})#f]{
  def pure[A](a: A) = f.pure(g.pure(a))
  
  def ap[A,B](fg: F[G[A => B]])(v: F[G[A]]): F[G[B]] = {
    val gap: G[A => B] => (G[A] => G[B]) = g.ap
    val gaToGb: F[G[A] => G[B]] = f.ap(f.pure(gap))(fg)
    f.ap(gaToGb)(v)
  }
}

// Tests
val listOptApp = compose(Applicative[List], Applicative[Option])
val toStr = (i: Int) => i.toString
val plus10ToStr = (i: Int) => (i+10).toString

assert(listOptApp.pure(1) == List(Some(1)))
assert(listOptApp.ap(Nil)(List(None)) == Nil)
assert(listOptApp.ap(List(Some(toStr)))(List(None, Some(2))) == List(None, Some("2")))
assert(listOptApp.ap(List(Some(toStr)))(List(Some(1), Some(2))) == List(Some("1"), Some("2")))
assert(listOptApp.ap(List(Some(toStr), Some(plus10ToStr)))(List(Some(1), Some(2))) == List(Some("1"), Some("2"), Some("11"), Some("12")))