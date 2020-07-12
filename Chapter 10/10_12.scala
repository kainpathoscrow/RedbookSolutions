// Implement  Foldable[List],  Foldable[IndexedSeq], and Foldable[Stream].

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

val foldableList = new Foldable[List] {
  def foldLeft[A, B](as: List[A], b: B)(f: (B, A) => B) =
    as.foldLeft(b)(f)
  def foldRight[A, B](as: List[A], b: Eval[B])(f: (A, Eval[B]) => Eval[B]) =
    as.foldRight(b)((a, b1) => f(a, b1))
  override def foldMap[A, B](as: List[A])(f: A => B)(implicit m: Monoid[B]): B =
    as.foldLeft(m.empty)((b, a) => m.combine(b, f(a)))
}

val foldableIndexedSeq = new Foldable[IndexedSeq] {
  def foldLeft[A, B](as: IndexedSeq[A], b: B)(f: (B, A) => B) =
    as.foldLeft(b)(f)
  def foldRight[A, B](as: IndexedSeq[A], b: Eval[B])(f: (A, Eval[B]) => Eval[B]) =
    as.foldRight(b)((a, b1) => f(a, b1))
  override def foldMap[A, B](as: IndexedSeq[A])(f: A => B)(implicit m: Monoid[B]): B =
    foldMapV(as, m)(f)
}

val foldableStream = new Foldable[Stream] {
  def foldLeft[A, B](as: Stream[A], b: B)(f: (B, A) => B) =
    as.foldLeft(b)(f)
  def foldRight[A, B](as: Stream[A], b: Eval[B])(f: (A, Eval[B]) => Eval[B]) =
    as.foldRight(b)((a, b1) => f(a, b1))
  override def foldMap[A, B](as: Stream[A])(f: A => B)(implicit m: Monoid[B]): B =
    as.foldLeft(m.empty)((b, a) => m.combine(b, f(a)))
}