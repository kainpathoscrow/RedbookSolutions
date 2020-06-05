// Write a function size that counts the number of nodes (leaves and branches) in a tree.

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

def size[A](tree: Tree[A]): Int = tree match {
  case Branch(left, right) => 1 + size(left) + size(right)
  case Leaf(_) => 1
}

assert(size(Leaf(1)) == 1)
assert(size(Branch(Leaf(1), Branch(Leaf(2), Leaf(3)))) == 5)