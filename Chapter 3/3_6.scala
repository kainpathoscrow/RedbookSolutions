/* Implement a function, init, that returns a List consisting of all but the last element of a List. 
So, given List(1,2,3,4), init will return List(1,2,3). */

def init[A](l: List[A]): List[A] = l match {
  case x0 :: Nil => Nil
  case x0 :: xs => x0 :: init(xs)
  case Nil => Nil
}

assert(init(Nil) == Nil)
assert(init(List(1)) == Nil)
assert(init(List(1,2)) == List(1))
assert(init(List(1,2,3,4)) == List(1,2,3))