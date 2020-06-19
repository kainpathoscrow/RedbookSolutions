// Generalize ones slightly to the function constant, which returns an infinite Stream of a given value. 
def constant[A](a: A): Stream[A] = a #:: constant(a)

assert(constant(1).take(3) == 1 #:: 1 #:: 1 #:: Stream.empty)