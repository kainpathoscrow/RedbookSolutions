// Use foldMap to detect whether a given IndexedSeq[Int] is ordered. 

import cats._

// Defs
def foldMapV[A,B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): B = 
  if (v.length > 1) {
    val (left, right) = v.splitAt(v.length / 2)
    m.combine(foldMapV(left, m)(f), foldMapV(right, m)(f))
  } else if (v.length == 1) {
    f(v(0))
  } else {
    m.empty
  }


// Solution
type IsOrdered = Boolean
type LeftInt = Int
type RightInt = Int
type ChechResult = (IsOrdered, LeftInt, RightInt)
val orderedMonoid = new Monoid[Option[ChechResult]] {
  def empty = None
  def combine(m1: Option[ChechResult], m2: Option[ChechResult]) = (m1, m2) match {
    case (Some((b1, l1, r1)), Some((b2, l2, r2))) => 
      Some(b1 && b2 && r1 <= l2, l1, r2)
    case _ => m1.orElse(m2)
  }
}

def ordered(ints: IndexedSeq[Int]): Boolean = {
  foldMapV(ints, orderedMonoid)(i => Some((true, i, i))).map(_._1).getOrElse(true)
}

// Test
assert(ordered(IndexedSeq()))
assert(ordered(IndexedSeq(1)))
assert(ordered(IndexedSeq(1,2)))
assert(!ordered(IndexedSeq(2,1)))
assert(ordered(IndexedSeq(1,2,3)))
assert(!ordered(IndexedSeq(2,1,3)))
assert(ordered(IndexedSeq(1,1,3)))
assert(ordered(IndexedSeq(1,3,3)))