// Write a more general stream-building function called unfold. It takes an initial state,
// and a function for producing both the next state and the next value in the generated stream.

def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
  case Some((a, s)) => a #:: unfold(s)(f)
  case None => Stream.empty
}

assert(unfold(0)(_ => None) == Stream.empty)
assert(unfold(0)(z => Some((z.toString, z+1))).take(3) == "0" #:: "1" #:: "2" #:: Stream.empty)