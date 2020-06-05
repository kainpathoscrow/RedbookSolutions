// Write a function size that counts the number of nodes (leaves and branches) in a tree.

def size[A](tree: Tree[A]): Int = tree match {
  case Branch(left, right) => 1 + size(left) + size(right)
  case Leaf(_) => 1
}