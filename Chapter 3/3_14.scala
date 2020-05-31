// Implement append in terms of either foldLeft or foldRight.

def append[A](as: List[A], bs: List[A]) = as.foldRight(bs)(_ +: _)

assert(append(Nil, List(1)) == List(1))
assert(append(List(1,2), List(3,4)) == List(1,2,3,4))
