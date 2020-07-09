// A function having the same argument and return type is sometimes called an endofunction.
// Write a monoid for endofunctions.

import cats._

def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
  def empty = a => a
  def combine(f1: A => A, f2: A => A) = a => f2(f1(a))
}