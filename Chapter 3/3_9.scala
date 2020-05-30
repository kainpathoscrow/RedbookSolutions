// Compute the length of a list using foldRight.

def len[A](as: List[A]): Int = as.foldRight(0)((_, acc) => acc + 1)

assert(len(Nil) == 0)
assert(len(List(1)) == 1)
assert(len(List(1,2,3)) == 3)