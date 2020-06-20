// Write fibs, from, constant, and ones in terms of unfold
import scala.collection.immutable.Stream._

def fibs: Stream[Int] = unfold((0, 1)){case (prev2, prev) => Some(prev2, (prev,(prev2+prev)))}

def from(n: Int): Stream[Int] = unfold(n)(n => Some(n, n+1))

def constant[A](a: A): Stream[A] = unfold(a)(a => Some(a, a))

def ones: Stream[Int] = unfold(1)(_ => Some(1, 1))

assert(fibs.take(6) == 0 #:: 1 #:: 1 #:: 2 #:: 3 #:: 5 #:: Stream.empty)

assert(from(0).take(3) == 0 #:: 1 #:: 2 #:: Stream.empty)

assert(constant(2).take(3) == 2 #:: 2 #:: 2 #:: Stream.empty)

assert(ones.take(3) == 1 #:: 1 #:: 1 #:: Stream.empty)
