// Give a Monoid instance for combining Option values.

import cats._

def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
  def empty = None
  def combine(a1: Option[A], a2: Option[A]) = a1.orElse(a2)
}