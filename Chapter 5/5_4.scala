// Implement forAll, which checks that all elements in the Stream match a given predicate.
// Your implementation should terminate the traversal as soon as it encounters a nonmatching value. 

def forAll[A](p: A => Boolean)(as: Stream[A]): Boolean = as match {
  case Stream.Empty => true
  case head #:: tail => if (p(head)) forAll(p)(tail) else false
}

assert(forAll[Int](_ < 0)(Stream.empty) == true)
assert(forAll[Int](_ < 0)(1 #:: 2 #:: Stream.empty) == false)
assert(forAll[Int](_ < 3)(1 #:: 2 #:: Stream.empty) == true)