// Write a function map that generalizes modifying each element in a list while maintaining the structure of the list.

def map[A,B](as: List[A])(f: A => B): List[B] = as.foldRight(List[B]())((cur, acc) => f(cur) :: acc)

assert(map(List(0,1,2))(_.toString) == List("0", "1", "2"))