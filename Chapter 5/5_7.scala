// Implement map, filter, append, and flatMap using foldRight. The append method should be non-strict in its argument.

def map[A, B](f: A => B)(as: Stream[A]): Stream[B] = as.foldRight(Stream.empty[B])((cur, acc) => f(cur) #:: acc) 

def filter[A](p: A => Boolean)(as: Stream[A]): Stream[A] = as.foldRight(Stream.empty[A])((cur, acc) => if (p(cur)) cur #:: acc else acc)

def append[A, B >: A](as: Stream[A])(bs: Stream[B]): Stream[B] = as.foldRight(bs)((cur, acc) => cur #:: acc)

def flatMap[A, B](f: A => Stream[B])(as: Stream[A]): Stream[B] = as.foldRight(Stream.empty[B])((cur, acc) => append(f(cur))(acc))

assert(map[Int, Int](_ + 1)(Stream.empty) == Stream.empty)
assert(map[Int, Int](_ + 1)(1 #:: 2 #:: Stream.empty) == 2 #:: 3 #:: Stream.empty)

assert(filter[Int](_ > 1)(Stream.empty) == Stream.empty)
assert(filter[Int](_ > 1)(1 #:: 2 #:: Stream.empty) == 2 #:: Stream.empty)

assert(append(Stream.empty)(Stream.empty) == Stream.empty)
assert(append(1 #:: 2 #:: Stream.empty)(3 #:: 4 #:: Stream.empty) == 1 #:: 2 #:: 3 #:: 4 #:: Stream.empty)

assert(flatMap[Int, Int](s => append(s #:: Stream.empty)(s #:: Stream.empty))(Stream.empty) == Stream.empty)
assert(flatMap[Int, Int](s => append(s #:: Stream.empty)(s #:: Stream.empty))(1 #:: Stream.empty) == 1 #:: 1 #:: Stream.empty)
