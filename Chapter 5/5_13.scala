/* Use  unfold to implement map, take, takeWhile, zipWith (as in chapter 3), and zipAll. 
The zipAll function should continue the traversal as long as either stream has more elements—it uses Option to indicate whether each stream has been exhausted. */

import scala.collection.immutable.Stream._

def map[A, B](f: A => B)(as: Stream[A]): Stream[B] = unfold(as)(cur => cur match {
  case head #:: tail => Some(f(head), tail)
  case Stream.Empty => None
})

def take[A](count: Int)(as: Stream[A]): Stream[A] = unfold((as, count)){ case (curList, curCount) => 
  if (curCount <= 0 || curList == Stream.empty) None else Some((curList.head, (curList.tail, curCount-1)))
}

def takeWhile[A](p: A => Boolean)(as: Stream[A]): Stream[A] = unfold(as)(cur => cur match {
  case head #:: tail => if (p(head)) Some((head, tail)) else None
  case Stream.Empty => None
})

def zipWith[A, B, C](f: (A, B) => C)(as: Stream[A])(bs: Stream[B]): Stream[C] = unfold((as, bs)) {
  case ((ha #:: ta), (hb #:: tb)) => Some((f(ha, hb), (ta, tb)))
  case _ => None
}

def zipAll[A, B](as: Stream[A])(bs: Stream[B]): Stream[(Option[A],Option[B])] = unfold(as, bs){
  case ((ha #:: ta), (hb #:: tb)) => Some(((Some(ha), Some(hb)), (ta, tb)))
  case ((ha #:: ta), Stream.Empty) => Some(((Some(ha), None), (ta, Stream.empty)))  
  case (Stream.Empty, (hb #:: tb)) => Some(((None, Some(hb)), (Stream.empty, tb)))  
  case (Stream.Empty, Stream.Empty) => None
}


def plus1(i: Int) = i + 1
def intPlusStr(i: Int, s: String) = i.toString + s
val stream1 = 1 #:: Stream.empty
val stream12 = 1 #:: 2 #:: Stream.empty
val stream12String = "1" #:: "2" #:: Stream.empty

assert(map[Int, Int](plus1)(Stream.empty) == Stream.empty)
assert(map[Int, Int](plus1)(stream12) == 2 #:: 3 #:: Stream.empty)

assert(take(2)(Stream.empty) == Stream.empty)
assert(take(1)(stream12) == 1 #::Stream.empty)
assert(take(3)(stream12) == 1 #:: 2 #:: Stream.empty)

assert(takeWhile[Int](_ < 0)(Stream.empty) == Stream.empty)
assert(takeWhile[Int](_ < 0)(stream12) == Stream.empty)
assert(takeWhile[Int](_ < 2)(stream12) == 1 #::Stream.empty)
assert(takeWhile[Int](_ < 3)(stream12) == stream12)

assert(zipWith[Int, String, String](intPlusStr)(Stream.empty)(Stream.empty) == Stream.empty)
assert(zipWith[Int, String, String](intPlusStr)(stream12)(Stream.empty) == Stream.empty)
assert(zipWith[Int, String, String](intPlusStr)(Stream.empty)(stream12String) == Stream.empty)
assert(zipWith[Int, String, String](intPlusStr)(stream1)(stream12String) == "11" #:: Stream.empty)
assert(zipWith[Int, String, String](intPlusStr)(stream12)(stream12String) == "11" #:: "22" #:: Stream.empty)


assert(zipAll[Int, String](Stream.empty)(Stream.empty) == Stream.empty)
assert(zipAll[Int, String](stream1)(Stream.empty) == (Some(1), None) #:: Stream.empty)
assert(zipAll[Int, String](stream1)(stream12String) == (Some(1), Some("1")) #:: (None, Some("2")) #:: Stream.empty)
assert(zipAll[Int, String](stream12)(stream12String) == (Some(1), Some("1")) #:: (Some(2), Some("2")) #:: Stream.empty)

