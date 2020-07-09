// Write foldLeft and foldRight using foldMap

import cats._

// Defs
def foldMap[A,B](as: List[A], m: Monoid[B])(f: A => B): B = {
  as.foldLeft(m.empty)((acc: B, cur: A) => m.combine(acc, f(cur)))
}
def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
  def empty = a => a
  def combine(f1: A => A, f2: A => A) = a => f2(f1(a))
}

// Solution

def foldLeft[A, B](as: List[A])(initial: B)(f: (B, A) => B): B = {
  val m = new Monoid[B => B] {
    def empty = b => b
    def combine(f1: B => B, f2: B => B) = b => f1(f2(b))
  }
  val foldedF = foldMap(as, m)(a => (b => f(b, a)))
  foldedF(initial)
}

def foldRight[A, B](as: List[A])(initial: B)(f: (A, B) => B): B = {
  val m = endoMonoid[B]
  val foldedF = foldMap(as, m)(a => (b => f(a, b)))
  foldedF(initial)
}

assert(foldLeft(List(1,2,3))(10)(_ - _) == 4)
assert(foldRight(List(1,2,3))(10)(_ - _) == -8)