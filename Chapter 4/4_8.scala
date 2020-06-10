/* In this implementation, map2 is only able to report one error, even if both the name and the age are invalid. 
What would you need to change in order to report both errors? Would you change map2 or the signature of mkPerson? 
Or could you create a new data type that captures this requirement better than Either does, with some additional structure? 
How would orElse, traverse, and sequence behave differently for that data type? */

sealed trait Validated[+E, +A] {
  def map[B](f: A => B): Validated[E, B] = this match {
    case i @ Invalid(_) => i
    case Valid(v) => Valid(f(v)) 
  }
  
  def flatMap[EE >: E, B](f: A => Validated[EE, B]): Validated[EE, B] = this match {
    case i @ Invalid(_) => i
    case Valid(v) => f(v)
  }
  
  def orElse[EE >: E, B >: A](b: => Validated[EE, B]): Validated[EE, B] = (this, b) match {
    case (Invalid(e1), Invalid(e2)) => Invalid(e1 ++ e2)
    case (Invalid(_), v @ Valid(_)) => v
    case (v @ Valid(_), _) => v
  }
  
  def map2[EE >: E, B, C](b: Validated[EE, B])(f: (A, B) => C): Validated[EE, C] = (this, b) match {
    case (Invalid(e1), Invalid(e2)) => Invalid(e1 ++ e2)
    case (i @ Invalid(_), Valid(_)) => i
    case (Valid(a), i @ Invalid(_)) => i
    case (Valid(a), Valid(b)) => Valid(f(a, b))
  }
}

case class Invalid[+E](value: Seq[E]) extends Validated[E, Nothing]
case class Valid[+A](value: A) extends Validated[Nothing, A]

def traverse[E, A, B](as: List[A])(f: A => Validated[E, B]): Validated[E, List[B]] = as match {
  case a1 :: as => (f(a1), traverse(as)(f)) match {
    case (Invalid(e1), Invalid(e2)) => Invalid(e1 ++ e2)
    case (i @ Invalid(_), Valid(_)) => i
    case (Valid(_), i @ Invalid(e)) => i
    case (Valid(v1), Valid(v2)) => Valid(v1 :: v2)
  }
  case Nil => Valid(Nil)
}
def sequence[E, A](es: List[Validated[E, A]]): Validated[E, List[A]] = traverse(es)(x => x)

assert((Invalid(List("Error")): Validated[String, Int]).map(_ + 1) == Invalid(List("Error")))
assert((Valid(0): Validated[String, Int]).map(_ + 1) == Valid(1))

assert((Invalid(List("Error")): Validated[String, Int]).flatMap(l => Invalid(List("Error1"))) == Invalid(List("Error")))
assert((Invalid(List("Error")): Validated[String, Int]).flatMap(r => Valid(r + 1)) == Invalid(List("Error")))
assert((Valid(0): Validated[String, Int]).flatMap(l => Invalid(List("Error"))) == Invalid(List("Error")))
assert((Valid(0): Validated[String, Int]).flatMap(r => Valid(r + 1)) == Valid(1))

assert((Invalid(List("Error")): Validated[String, Int]).orElse(Invalid(List("Error1"))) == Invalid(List("Error", "Error1")))
assert((Invalid(List("Error")): Validated[String, Int]).orElse(Valid(0)) == Valid(0))
assert((Valid(0): Validated[String, Int]).orElse(Invalid(List("Error"))) == Valid(0))
assert((Valid(0): Validated[String, Int]).orElse(Valid(1)) == Valid(0))

assert((Invalid(List("Error")): Validated[String, Int]).map2(Invalid(List("Error1")))(_ + _) == Invalid(List("Error", "Error1")))
assert((Invalid(List("Error")): Validated[String, Int]).map2(Valid(0))(_ + _) == Invalid(List("Error")))
assert((Valid(1): Validated[String, Int]).map2(Invalid(List("Error")))(_ + _) == Invalid(List("Error")))
assert((Valid(1): Validated[String, Int]).map2(Valid(2))(_ + _) == Valid(3))

assert(sequence(Nil) == Valid(Nil))
assert(sequence(List(Invalid(List("Error")), Invalid(List("Error1")))) == Invalid(List("Error", "Error1")))
assert(sequence(List(Invalid(List("Error")), Valid(0))) == Invalid(List("Error")))
assert(sequence(List(Valid(0), Invalid(List("Error")))) == Invalid(List("Error")))
assert(sequence(List(Valid(1), Valid(2))) == Valid(List(1, 2)))