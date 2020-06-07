// Write a generic function map2 that combines two Option values using a binary function. 
// If either Option value is None, then the return value is too. Here is its signature:
def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = (a, b) match {
  case (None, _) => None
  case (_, None) => None
  case (Some(x), Some(y)) => Some(f(x, y))
}

assert(map2(None: Option[Int], None: Option[Int])(_ + _) == None)
assert(map2(None: Option[Int], Some(2))(_ + _) == None)
assert(map2(Some(1), None: Option[Int])(_ + _) == None)
assert(map2(Some(1), Some(2))(_ + _) == Some(3))