// Use foldRight to implement takeWhile.


def takeWhile[A](p: A => Boolean)(as: Stream[A]): Stream[A] = 
  as.foldRight(Stream.empty[A])((cur, acc) => if (p(cur)) cur #:: acc else Stream.empty)

assert(takeWhile[Int](_ < 1)(Stream.empty) == Stream.empty)
assert(takeWhile[Int](_ < 1)(1 #:: 2 #:: Stream.empty) == Stream.empty)
assert(takeWhile[Int](_ < 2)(1 #:: 2 #:: Stream.empty) == 1 #:: Stream.empty)
assert(takeWhile[Int](_ < 3)(1 #:: 2 #:: Stream.empty) == 1 #:: 2 #:: Stream.empty)
