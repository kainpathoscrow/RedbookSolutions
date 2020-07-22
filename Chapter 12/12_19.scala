// Implement the composition of two Traverse instances. 

import cats._
import cats.data._
import cats.implicits._

// Solution
def compose[F[_]:Traverse, G[_]:Traverse]: Traverse[({type f[x] = F[G[x]]})#f] = new Traverse[({type f[x] = F[G[x]]})#f] {
  def traverse[H[_]:Applicative, A, B](fga: F[G[A]])(h: A => H[B]): H[F[G[B]]] = {
    Traverse[F].traverse(fga)(ga => Traverse[G].traverse(ga)(h))
  }
  
  def foldLeft[A, B](fa: F[G[A]], b: B)(f: (B, A) => B): B = ???
  def foldRight[A, B](fa: F[G[A]], lb: cats.Eval[B])(f: (A, cats.Eval[B]) => cats.Eval[B]): cats.Eval[B] = ???  
}

// Tests
implicit val optionListTraverse = compose[Option, List]
assert(optionListTraverse.traverse(None: Option[List[Int]])(i => Right(i): Either[Unit, Int]) == Right(None))
assert(optionListTraverse.traverse(Some(List(1,2,3)): Option[List[Int]])(i => Left(()): Either[Unit, Int]) == Left(()))
assert(optionListTraverse.traverse(Some(List(1,2,3)): Option[List[Int]])(i => Right(i): Either[Unit, Int]) == Right(Some(List(1,2,3))))

