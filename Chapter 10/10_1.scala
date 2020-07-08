// Give Monoid instances for integer addition and multiplication as well as the Boolean operators. 

import cats._

val intAddition: Monoid[Int] = new Monoid[Int] {
  def empty = 0
  def combine(a: Int, b: Int) = a + b
}

val intMultiplication: Monoid[Int] = new Monoid[Int] {
  def empty = 1
  def combine(a: Int, b: Int) = a * b
}

val booleanOr: Monoid[Boolean] = new Monoid[Boolean] {
  def empty = false
  def combine(a: Boolean, b: Boolean) = a || b
}

val booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {
  def empty = true
  def combine(a: Boolean, b: Boolean) = a && b
}
