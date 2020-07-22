// Write reverse function, and think about what it means for List, Tree, and other traversable functors. 

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

def toList[A,F[_]:Traverse](fa: F[A]): List[A] =
  mapAccum(fa, List[A]())((a, s) => ((), a :: s))._2.reverse

case class Tree[+A](head: A, tail: List[Tree[A]])
implicit val treeTraverse = new Traverse[Tree] {
  def traverse[G[_]: Applicative, A, B](aTree: Tree[A])(f: A => G[B]): G[Tree[B]] = {
    val ga = implicitly[Applicative[G]]
    val gHead = f(aTree.head)
    val gTail = Traverse[List].traverse(aTree.tail)((a: Tree[A]) => traverse(a)(f))
    ga.map2(gHead, gTail)((h,t) => Tree(h, t))    
  }
       
  def foldLeft[A, B](fa: Tree[A],b: B)(f: (B, A) => B): B = ???
  def foldRight[A, B](fa: Tree[A],lb: cats.Eval[B])(f: (A, cats.Eval[B]) => cats.Eval[B]): cats.Eval[B] = ???
}

// Solution
def reverse[A,F[_]:Traverse](fa: F[A]): F[A] = 
  mapAccum(fa, toList(fa).reverse){
    case (_, head :: tail) => (head, tail)
  }._1

// Tests
assert(reverse(List(1,2,3)) == List(3,2,1))
assert(reverse(Tree(1, List(Tree(2, Nil), Tree(3, Nil)))) == Tree(3, List(Tree(2, Nil), Tree(1, Nil))))