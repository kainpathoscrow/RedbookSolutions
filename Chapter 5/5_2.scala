// Write the function take(n) for returning the first n elements of a Stream, and drop(n) for skipping the first n elements of a Stream.


def take[A](as: Stream[A], n: Int): Stream[A] = if (n > 0) as match {
  case Stream.Empty => Stream.empty
  case head #:: tail => head #:: take(tail, n - 1)
} else Stream.empty

def drop[A](as: Stream[A], n: Int): Stream[A] = if (n > 0) as match {
  case Stream.Empty => Stream.empty
  case head #:: tail => drop(tail, n - 1)
} else as

assert(take(Stream.empty, 2) == Stream.empty)
assert(take(1 #:: 2 #:: Stream.empty, 1) == 1 #::Stream.empty)
assert(take(1 #:: 2 #:: Stream.empty, 3) == 1 #:: 2 #:: Stream.empty)

assert(drop(Stream.empty, 2) == Stream.empty)
assert(drop(1 #:: 2 #:: Stream.empty, 1) == 2 #::Stream.empty)
assert(drop(1 #:: 2 #:: Stream.empty, 3) == Stream.empty)