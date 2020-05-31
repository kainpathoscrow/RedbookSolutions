// Implement append in terms of either foldLeft or foldRight.

def append[A](as: List[A], z: A) = as.foldRight(List(z))(_ +: _)

assert(append(Nil, 1) == List(1))
assert(append(List(1,2), 3) == List(1,2,3))
