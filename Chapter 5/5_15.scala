/* Implement tails using unfold. For a given Stream, tails returns the Stream of suffixes of the input sequence, starting with the original Stream. 
For example, givenStream(1,2,3), it would return Stream(Stream(1,2,3), Stream(2,3), Stream(3),Stream()).*/ 

import scala.collection.immutable.Stream._

def tails[A](as: Stream[A]): Stream[Stream[A]] = {
  val tailsInit: Stream[Stream[A]] = unfold(as){ cur => cur match {
    case head #:: tail => Some(cur, tail)
    case Stream.Empty => None
  }}
  
  tailsInit.append(Stream(Stream.empty))
}

val stream3 = 3 #:: Stream.empty
val stream23 = 2 #:: 3 #:: Stream.empty
val stream123 = 1 #:: 2 #:: 3 #:: Stream.empty

assert(tails(Stream.empty) == Stream.empty #:: Stream.empty)
assert(tails(1 #:: 2 #:: 3 #:: Stream.empty) == stream123 #:: stream23 #:: stream3 #:: Stream.empty #:: Stream.empty)