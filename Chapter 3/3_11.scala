// Write sum, product, and a function to compute the length of a list using foldLeft.

def sum(as: List[Int]): Int = as.foldLeft(0)(_ + _)
assert(sum(Nil) == 0)
assert(sum(List(1,2,3,4)) == 10)

def product(as: List[Int]): Int = as.foldLeft(1)(_ * _)
assert(product(Nil) == 1)
assert(product(List(1,2,3,4)) == 24)

def length[A](as: List[A]): Int = as.foldLeft(0)((acc, cur) =>  acc + 1)
assert(length(Nil) == 0)
assert(length(List(1,2,3,4)) == 4)