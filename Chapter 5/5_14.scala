/* Hard: Implement startsWith using functions you’ve written. 
It should check if one Stream is a prefix of another. 
For instance, Stream(1,2,3) startsWith Stream(1,2) would be true. */

import scala.collection.immutable.Stream._

def zipAll[A, B](as: Stream[A])(bs: Stream[B]): Stream[(Option[A],Option[B])] = unfold(as, bs){
  case ((ha #:: ta), (hb #:: tb)) => Some(((Some(ha), Some(hb)), (ta, tb)))
  case ((ha #:: ta), Stream.Empty) => Some(((Some(ha), None), (ta, Stream.empty)))  
  case (Stream.Empty, (hb #:: tb)) => Some(((None, Some(hb)), (Stream.empty, tb)))  
  case (Stream.Empty, Stream.Empty) => None
}

def startsWith[A](as: Stream[A])(s: Stream[A]): Boolean = 
  zipAll[A, A](as)(s).forall { 
    case (Some(s1), Some(s2)) => s1 == s2 
    case (Some(_), None) => true
    case (None, Some(_)) => false
    case _ => true
  }

val stream1 = 1 #:: Stream.empty
val stream12 = 1 #:: 2 #:: Stream.empty

assert(startsWith(Stream.empty)(Stream.empty))
assert(startsWith(stream12)(Stream.empty))
assert(!startsWith(Stream.empty)(stream12))
assert(startsWith(stream12)(stream1))
assert(!startsWith(stream1)(stream12))