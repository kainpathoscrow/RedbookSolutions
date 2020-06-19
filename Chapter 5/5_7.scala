// Implement map, filter, append, and flatMap using foldRight. The append method should be non-strict in its argument.

def map[A, B](as: Stream[A])(f: A => B): Stream[B] = as.foldRight(Stream.empty[B])((cur, acc) => f(cur) #:: acc) 

def filter[A](as: Stream[A])(p: A => Boolean): Stream[A] = as.foldRight(Stream.empty[A])((cur, acc) => if (p(cur)) cur #:: acc else acc)

def append[A, B >: A](as: Stream[A])(bs: Stream[B]): Stream[B] = as.foldRight(bs)((cur, acc) => cur #:: acc)

def flatMap[A, B](as: Stream[A])(f: A => Stream[B]): Stream[B] = as.foldRight(Stream.empty[B])((cur, acc) => append(f(cur))(acc))

assert(map(Stream.empty[Int])(_ + 1) == Stream.empty[Int])
assert(map(1 #:: 2 #:: Stream.empty)(_ + 1) == 2 #:: 3 #:: Stream.empty)

assert(filter(Stream.empty[Int])(_ > 1) == Stream.empty[Int])
assert(filter(1 #:: 2 #:: Stream.empty)(_ > 1) == 2 #:: Stream.empty)

assert(append(Stream.empty)(Stream.empty) == Stream.empty)
assert(append(1 #:: 2 #:: Stream.empty)(3 #:: 4 #:: Stream.empty) == 1 #:: 2 #:: 3 #:: 4 #:: Stream.empty)

assert(flatMap(Stream.empty[Int])(s => append(s #:: Stream.empty)(s #:: Stream.empty)) == Stream.empty[Int])
assert(flatMap(1 #:: Stream.empty)(s => append(s #:: Stream.empty)(s #:: Stream.empty)) == 1 #:: 1 #:: Stream.empty)
