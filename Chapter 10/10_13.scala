//Recall the binary Tree data type from chapter 3. Implement a Foldable instance for it. 

import cats._ 

// Defs
sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

// Solution
val treeFoldable = new Foldable[Tree] {
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
 
  override def foldMap[A, B](as: Tree[A])(f: A => B)(implicit m: Monoid[B]): B = as match {
    case Leaf(a) => f(a)
    case Branch(l, r) => m.combine(foldMap(l)(f), foldMap(r)(f))
  }
}

// Tests
implicit val intAddition: Monoid[Int] = new Monoid[Int] {
  def empty = 0
  def combine(a: Int, b: Int) = a + b
}
val tree = Branch(Branch(Leaf(1),Leaf(2)), Leaf(3))
val treeStr = Branch(Branch(Leaf("1"),Leaf("2")), Leaf("3"))


assert(treeFoldable.foldLeft(tree, 0)((x, y) => x - y) == -6)
assert(treeFoldable.foldRight(tree, Eval.now(0))((x, y) => Eval.now(x - y.value)).value == 2)
assert(treeFoldable.foldMap(treeStr)(_.toInt) == 6)