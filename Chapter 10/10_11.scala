// Use the WC monoid to implement a function that counts words in a String by recursively splitting it into substrings and counting the words in those substrings.

import cats._

// Defs 
sealed trait WC
case class Stub(chars: String) extends WC
case class Part(lStub: String, words: Int, rStub: String) extends WC

val wcMonoid = new Monoid[WC] {
  def empty = Stub("")
  def combine(w1: WC, w2: WC) = (w1, w2) match {
    case (Stub(s1), Stub(s2)) => Stub(s1 + s2)
    case (Stub(s), Part(l, w, r)) => Part(s+l, w, r)
    case (Part(l, w, r), Stub(s)) => Part(l, w, r+s)
    case (Part(l1, w1, r1), Part(l2, w2, r2)) =>
      val r1l2Count = if ((r1+l2) == "") 0 else 1
      Part(l1, w1+w2+r1l2Count, r2)
  }
}

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
def stubToWordCount(s: String) = if (s == "") 0 else 1
def countWords(s: String): Int = {
  val foldResult = foldMapV(s.toIndexedSeq, wcMonoid)(c => if (c == ' ') Part("", 0, "") else Stub(c.toString))
  foldResult match {
    case Stub(s) => stubToWordCount(s)
    case Part(l, w, r) => stubToWordCount(l) + stubToWordCount(r) + w
  }
}

// Tests
assert(countWords("") == 0)
assert(countWords("a") == 1)
assert(countWords("aa") == 1)
assert(countWords("aa a") == 2)
assert(countWords("aa aa") == 2)
assert(countWords("aa      aa") == 2)
assert(countWords("aa    aaa   a aaaa") == 4)
assert(countWords(" aa    aaa   a aaaa ") == 4)