// The real power of monoids comes from the fact that they compose. Prove it.

import cats._

// Solution
def productMonoid[A,B](A: Monoid[A], B: Monoid[B]): Monoid[(A,B)] = new Monoid[(A, B)] {
  def empty = (A.empty, B.empty)
  def combine(x: (A, B), y: (A, B)) = (x, y) match {
    case ((a1, b1), (a2, b2)) => (A.combine(a1, a2), B.combine(b1, b2))
  }
}

// Test
val intAddition: Monoid[Int] = new Monoid[Int] {
  def empty = 0
  def combine(a: Int, b: Int) = a + b
}
val booleanOr: Monoid[Boolean] = new Monoid[Boolean] {
  def empty = false
  def combine(a: Boolean, b: Boolean) = a || b
}

val intBoolMonoid = productMonoid(intAddition, booleanOr)

assert(intBoolMonoid.empty == (0, false))
assert(intBoolMonoid.combine((1,false), (2, true)) == (3, true))