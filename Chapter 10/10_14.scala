// Write a Foldable[Option] instance.

import cats._ 

// Solution
val optionF = new Foldable[Option] {
 def foldLeft[A, B](as: Option[A], b: B)(f: (B, A) => B) = as match {
   case Some(a) => f(b, a)
   case None => b
 }
    
  def foldRight[A, B](as: Option[A], b: Eval[B])(f: (A, Eval[B]) => Eval[B]) = as match {
   case Some(a) => f(a, b)
   case None => b
 }
 
  override def foldMap[A, B](as: Option[A])(f: A => B)(implicit m: Monoid[B]): B = as match {
   case Some(a) => f(a)
   case None => m.empty
 }
}

// Tests
implicit val intAddition: Monoid[Int] = new Monoid[Int] {
  def empty = 0
  def combine(a: Int, b: Int) = a + b
}
def addAEvalB(a: Int, b: Eval[Int]) = Eval.now(a + b.value)

assert(optionF.foldLeft(None: Option[Int], 1)((a, b) => a + b) == 1)
assert(optionF.foldLeft(Some(2), 1)((a, b) => a + b) == 3)

assert(optionF.foldRight(None: Option[Int], Eval.now(1))(addAEvalB).value == 1)
assert(optionF.foldRight(Some(2), Eval.now(1))(addAEvalB).value == 3)

assert(optionF.foldMap(None: Option[String])(_.toInt) == 0)
assert(optionF.foldMap(Some(1))(_.toInt) == 1)