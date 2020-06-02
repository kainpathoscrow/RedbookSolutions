// Write a function flatMap that works like map except that the function given will return a list instead of a single result, 
// and that list should be inserted into the final resulting list.

def concat[A](lists: List[List[A]]): List[A] = // same as in 3_15
  lists.foldRight(List[A]())((acc, cur) => acc ::: cur)

def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] = concat(as.map(f))

assert(flatMap(List[Int]())(x => List(x, x+1)) == Nil)
assert(flatMap(List(1,2,3))(x => List(x, x+1)) == List(1,2,2,3,3,4))