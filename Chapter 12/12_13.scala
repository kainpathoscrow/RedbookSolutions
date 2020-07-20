// Write Traverse instances for List, Option, and Tree.

import cats._
import cats.implicits._

// Defs
case class Tree[+A](head: A, tail: List[Tree[A]])

// Solution
val listTraverse = new Traverse[List] {
  def traverse[G[_]: Applicative, A, B](as: List[A])(f: A => G[B]): G[List[B]] = {
    val ga = implicitly[Applicative[G]]
    as.foldRight(ga.pure(Nil: List[B]))((cur, acc) => ga.map2(f(cur), acc)(_ :: _))
  }
  
  // Required by cats, but not by redbook
  def foldLeft[A, B](fa: List[A],b: B)(f: (B, A) => B): B = ???
  def foldRight[A, B](fa: List[A],lb: cats.Eval[B])(f: (A, cats.Eval[B]) => cats.Eval[B]): cats.Eval[B] = ???
}

val optionTraverse = new Traverse[Option] {
  def traverse[G[_]: Applicative, A, B](aOpt: Option[A])(f: A => G[B]): G[Option[B]] = {
    val ga = implicitly[Applicative[G]]
    aOpt match {
      case Some(a) => ga.map(f(a))(a1 => Some(a1))
      case None => ga.pure(None)
    }
  }
       
  def foldLeft[A, B](fa: Option[A],b: B)(f: (B, A) => B): B = ???
  def foldRight[A, B](fa: Option[A],lb: cats.Eval[B])(f: (A, cats.Eval[B]) => cats.Eval[B]): cats.Eval[B] = ???
}

val treeTraverse = new Traverse[Tree] {
  def traverse[G[_]: Applicative, A, B](aTree: Tree[A])(f: A => G[B]): G[Tree[B]] = {
    val ga = implicitly[Applicative[G]]
    val gHead = f(aTree.head)
    val gTail = listTraverse.traverse(aTree.tail)((a: Tree[A]) => traverse(a)(f))
    ga.map2(gHead, gTail)((h,t) => Tree(h, t))    
  }
       
  def foldLeft[A, B](fa: Tree[A],b: B)(f: (B, A) => B): B = ???
  def foldRight[A, B](fa: Tree[A],lb: cats.Eval[B])(f: (A, cats.Eval[B]) => cats.Eval[B]): cats.Eval[B] = ???
}