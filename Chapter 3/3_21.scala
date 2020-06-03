// Use flatMap to implement filter.

def filter[A](as: List[A])(f: A => Boolean): List[A] = as.flatMap(a => if (f(a)) List(a) else Nil)

assert(filter(List[Int]())(_ % 2 == 0) == List[Int]())
assert(filter(List(1,2,3,4,5,6))(_ % 2 == 0) == List(2,4,6))