/* Implement a foldMap for IndexedSeq.
Your implementation should use the strategy of splitting the sequence in two, recursively processing each half, 
and then adding the answers together with the monoid. */

import cats._

def foldMapV[A,B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): B = 
  if (v.length > 1) {
    val (left, right) = v.splitAt(v.length / 2)
    m.combine(foldMapV(left, m)(f), foldMapV(right, m)(f))
  } else if (v.length == 1) { // to prevent endless recursion
    f(v(0))
  } else {
    m.empty
  }

// Test
val intAddition: Monoid[Int] = new Monoid[Int] {
  def empty = 0
  def combine(a: Int, b: Int) = a + b
}

assert(foldMapV(IndexedSeq[String](), intAddition)(_.toInt) == 0)
assert(foldMapV(IndexedSeq("1"), intAddition)(_.toInt) == 1)
assert(foldMapV(IndexedSeq("1", "2", "3"), intAddition)(_.toInt) == 6)
