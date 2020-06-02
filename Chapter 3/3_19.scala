// Write a function filter that removes elements from a list unless they satisfy a given predicate. Use it to remove all odd numbers from a List[Int].

def filter[A](as: List[A])(f: A => Boolean): List[A] = as.foldRight(List[A]())((cur, acc) => if (f(cur)) cur :: acc else acc)

assert(filter(List[Int]())(_ % 2 == 0) == List[Int]())
assert(filter(List(1,2,3,4,5,6))(_ % 2 == 0) == List(2,4,6))