// Any Foldable structure can be turned into a List. Write this conversion in a generic way:

import cats._

// Solution

def toList[A, F[_]](fa: F[A])(implicit f: Foldable[F]): List[A] = 
  f.foldLeft(fa, List[A]())((acc, cur) => acc :+ cur)
  
// Tests
sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
implicit val treeFoldable = new Foldable[Tree] {
 def foldLeft[A, B](as: Tree[A], b: B)(f: (B, A) => B) = as match {
   case Leaf(a) => f(b, a)
   case Branch(l, r) => {
     val leftValue = foldLeft(l, b)(f)
     foldLeft(r, leftValue)(f)
   }
 }
  def foldRight[A, B](as: Tree[A], b: Eval[B])(f: (A, Eval[B]) => Eval[B]) = as match {
   case Leaf(a) => f(a, b)
   case Branch(l, r) => {
     val rightValue = foldRight(r, b)(f)
     foldRight(l, rightValue)(f)
   }
 }
}

val tree: Tree[Int] = Branch(Branch(Leaf(1),Leaf(2)), Leaf(3))
assert(toList(tree) == List(1,2,3))