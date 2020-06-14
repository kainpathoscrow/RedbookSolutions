// Write the function takeWhile for returning all starting elements of a Stream that match the given predicate. 

def takeWhile[A](p: A => Boolean)(as: Stream[A]): Stream[A] = as match {
  case Stream.Empty => Stream.empty
  case head #:: tail => if (p(head)) head #:: takeWhile(p)(tail) else Stream.empty
}

assert(takeWhile[Int](_ < 0)(Stream.empty) == Stream.empty)
assert(takeWhile[Int](_ < 0)(1 #:: 2 #:: Stream.empty) == Stream.empty)
assert(takeWhile[Int](_ < 2)(1 #:: 2 #:: Stream.empty) == 1 #::Stream.empty)
assert(takeWhile[Int](_ < 3)(1 #:: 2 #:: Stream.empty) == 1 #:: 2 #:: Stream.empty)
