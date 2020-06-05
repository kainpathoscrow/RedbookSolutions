// Write a function depth that returns the maximum path length from the root of a tree to any leaf.

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

def depth[A](tree: Tree[A]): Int = tree match {
  case Branch(left, right) => 1 + depth(left).max(depth(right))
  case Leaf(_) => 0
}

assert(depth(Leaf(1)) == 0)
assert(depth(Branch(Leaf(3), Branch(Leaf(2), Leaf(1)))) == 2)