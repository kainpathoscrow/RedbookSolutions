/* Write a function that concatenates a list of lists into a single list. Its runtime should be linear in the total length of all lists. */

def concat[A](lists: List[List[A]]): List[A] = 
  lists.foldRight(List[A]())((acc, cur) => acc ::: cur)

assert(concat(List(Nil, Nil)) == Nil)
assert(concat(List(List(1,2), Nil, List(3))) == List(1,2,3))