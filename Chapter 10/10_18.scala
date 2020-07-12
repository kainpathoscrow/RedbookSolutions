// Use monoids to compute a “bag” from an IndexedSeq. 

import cats._

// Defs
def foldMapV[A,B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): B = 
  if (v.length > 1) {
    val (left, right) = v.splitAt(v.length / 2)
    m.combine(foldMapV(left, m)(f), foldMapV(right, m)(f))
  } else if (v.length == 1) { // to prevent endless recursion
    f(v(0))
  } else {
    m.empty
  }
val intAddition: Monoid[Int] = new Monoid[Int] {
  def empty = 0
  def combine(a: Int, b: Int) = a + b
}
def mapMergeMonoid[K,V](V: Monoid[V]): Monoid[Map[K, V]] =
  new Monoid[Map[K, V]] {
    def empty = Map[K,V]()
    def combine(a: Map[K, V], b: Map[K, V]) =
      (a.keySet ++ b.keySet).foldLeft(Map[K,V]()) { (acc,k) =>
        acc.updated(k, V.combine(a.getOrElse(k, V.empty),
                            b.getOrElse(k, V.empty)))
      }
  }

// Solution
def bag[A](as: IndexedSeq[A]): Map[A, Int] = {
  val mapMergeIntAdd = mapMergeMonoid[A, Int](intAddition)
  foldMapV(as, mapMergeIntAdd)(a => Map(a -> 1))
}

assert(bag(Vector("a", "rose", "is", "a", "rose")) ==  Map("a" -> 2, "rose" -> 2, "is" -> 1))