// Write a monoid instance for WC and make sure that it meets the monoid laws.

import cats._

// Defs
sealed trait WC
case class Stub(chars: String) extends WC
case class Part(lStub: String, words: Int, rStub: String) extends WC

// Solution
val wcMonoid = new Monoid[WC] {
  def empty = Stub("")
  def combine(w1: WC, w2: WC) = (w1, w2) match {
    case (Stub(s1), Stub(s2)) => Stub(s1 + s2)
    case (Stub(s), Part(l, w, r)) => Parts(s+l, w, r)
    case (Part(l, w, r), Stub(s)) => Parts(l, w, r+s)
    case (Part(l1, w1, r1), Part(l2, w2, r2)) =>
      val r1l2Count = if (r1 == "" && l2 == "") 0 else 1
      Part(l1, w1+w2+r1l2Count, r2)
  } 
}