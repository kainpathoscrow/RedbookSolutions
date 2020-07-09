// Implement foldMap.

import cats._

// Solution
def foldMap[A,B](as: List[A], m: Monoid[B])(f: A => B): B = {
  as.foldLeft(m.empty)((acc: B, cur: A) => m.combine(acc, f(cur)))
}

// Test
val intAddition: Monoid[Int] = new Monoid[Int] {
  def empty = 0
  def combine(a: Int, b: Int) = a + b
}

assert(foldMap(List("1", "2", "3"), intAddition)(_.toInt) == 6)