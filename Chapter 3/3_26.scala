// Write a function maximum that returns the maximum element in a Tree[Int]

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

def maximum(tree: Tree[Int]): Int = tree match {
  case Branch(left, right) => maximum(left).max(maximum(right))
  case Leaf(x) => x
}

assert(maximum(Leaf(1)) == 1)
assert(maximum(Branch(Leaf(3), Branch(Leaf(2), Leaf(1)))) == 3)