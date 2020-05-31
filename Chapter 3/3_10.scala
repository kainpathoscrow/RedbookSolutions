// Write another general list-recursion function, foldLeft, that is tail-recursive

def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B = as match {
  case x :: xs => foldLeft(xs, f(z, x))(f)
  case Nil => z
}

assert(foldLeft(Nil, "a")((a, b) => a+b) == "a")
assert(foldLeft(List(1,2,3), 0)((a, b) => a+b) == 6)
assert(foldLeft(List(1,2,3), 0)((a, b) => a-b) == -6) // (((0 - 1) - 2) - 3)