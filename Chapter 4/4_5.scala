// Implement this [traverse] function. 
// It’s straightforward to do using map and sequence, but try for a more efficient implementation that only looks at the list once. 
// In fact, implement sequence in terms of traverse.


def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = a match {
  case a1 :: as => for {
    b1 <- f(a1)
    bs <- traverse(as)(f)
  } yield b1 :: bs
  case Nil => Some(Nil)
}

def sequence[A](a: List[Option[A]]): Option[List[A]] = traverse(a)(x => x)

assert(sequence(Nil) == Some(Nil))
assert(sequence(List(None, Some(1))) == None)
assert(sequence(List(Some(1), None)) == None)
assert(sequence(List(Some(1), Some(2))) == Some(List(1, 2)))