// Write a function map, analogous to the method of the same name on List, that modifies each element in a tree with a given function.

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = tree match {
  case Branch(left, right) => Branch(map(left)(f), map(right)(f))
  case Leaf(x) => Leaf(f(x))
}

assert(map(Leaf(1))(_.toString) == Leaf("1"))
assert(map(Branch(Leaf(3), Branch(Leaf(2), Leaf(1))))(_.toString) == Branch(Leaf("3"), Branch(Leaf("2"), Leaf("1"))))