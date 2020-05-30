// Write a function that returns the reverse of a list (given List(1,2,3) it returns List(3,2,1))

def reverse[T](as: List[T]): List[T] = as.foldRight(List[T]())((x, y) => y :+ x)

assert(reverse(Nil) == Nil)
assert(reverse(List(1,2,3)) == List(3,2,1))