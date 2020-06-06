// Generalize size, maximum, depth, and map, writing a new function fold that abstracts over their similarities. 
// Reimplement them in terms of this more general function. 

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

def fold[A, B](tree: Tree[A])(fLeaf: A => B)(fBranch: (B, B) => B): B = tree match {
  case Branch(left, right) => fBranch(fold(left)(fLeaf)(fBranch), fold(right)(fLeaf)(fBranch))
  case Leaf(x) => fLeaf(x)
}

def size[A](tree: Tree[A]): Int = fold(tree)(_ => 1)(1 + _ + _)
assert(size(Leaf(1)) == 1)
assert(size(Branch(Leaf(1), Branch(Leaf(2), Leaf(3)))) == 5)

def maximum(tree: Tree[Int]): Int = fold(tree)(x => x)(_.max(_))
assert(maximum(Leaf(1)) == 1)
assert(maximum(Branch(Leaf(3), Branch(Leaf(2), Leaf(1)))) == 3)

def depth[A](tree: Tree[A]): Int = fold(tree)(_ => 0)(1 + _.max(_))
assert(depth(Leaf(1)) == 0)
assert(depth(Branch(Leaf(3), Branch(Leaf(2), Leaf(1)))) == 2)

def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = fold(tree)(x => Leaf(f(x)): Tree[B])(Branch(_, _))
assert(map(Leaf(1))(_.toString) == Leaf("1"))
assert(map(Branch(Leaf(3), Branch(Leaf(2), Leaf(1))))(_.toString) == Branch(Leaf("3"), Branch(Leaf("2"), Leaf("1"))))