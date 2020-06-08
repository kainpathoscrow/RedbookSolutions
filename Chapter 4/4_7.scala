// Implement sequence and traverse for Either. These should return the first error that’s encountered, if there is one.

def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] = as match {
  case a1 :: as => for {
    b1 <- f(a1)
    bs <- traverse(as)(f)
  } yield b1 :: bs
  case Nil => Right(Nil)
}

def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = traverse(es)(x => x)

assert(sequence(Nil) == Right(Nil))
assert(sequence(List(Left("Error"), Right(0))) == Left("Error"))
assert(sequence(List(Right(0), Left("Error"))) == Left("Error"))
assert(sequence(List(Right(1), Right(2))) == Right(List(1, 2)))