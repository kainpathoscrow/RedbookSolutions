// Hard: Implement headOption using foldRight.

def headOption[A](as: Stream[A]): Option[A] = as.foldRight(None: Option[A])((cur, _) => Some(cur))

assert(headOption(Stream.empty) == None)
assert(headOption(1 #:: 2 #:: 3 #:: Stream.empty) == Some(1))