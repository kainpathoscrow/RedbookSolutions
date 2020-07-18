// Write an Applicative instance for Validation that accumulates errors in Failure.

import cats._
import cats.implicits._
import cats.data._
import cats.data.Validated._

// Solution
def validationApplicative[E: Monoid]: Applicative[({type f[x] = Validated[E,x]})#f] = new Applicative[({type f[x] = Validated[E,x]})#f] {
  def pure[A](a: A) = Valid(a)
  
  def ap[A,B](vab: Validated[E, A => B])(va: Validated[E, A]): Validated[E, B] = (vab, va) match {
    case(Valid(ab), Valid(a)) => Valid(ab(a))
    case(Valid(_), Invalid(e)) => Invalid(e)
    case(Invalid(e), Valid(_)) => Invalid(e)
    case(Invalid(e1), Invalid(e2)) => Invalid(Monoid[E].combine(e1, e2))
  }
}

// Tests
def validationStringList = validationApplicative[List[String]]

def invalidInt = Invalid(List("IntErr")): Validated[List[String], Int]
def validInt = Valid(1): Validated[List[String], Int]
def invalidF = Invalid(List("FunctionErr")): Validated[List[String], Int => String]
def validF = Valid((i: Int) => i.toString): Validated[List[String], Int => String]

assert(validationStringList.ap(invalidF)(invalidInt) == Invalid(List("FunctionErr", "IntErr")))
assert(validationStringList.ap(invalidF)(validInt) == Invalid(List("FunctionErr")))
assert(validationStringList.ap(validF)(invalidInt) == Invalid(List("IntErr")))
assert(validationStringList.ap(validF)(validInt) == Valid("1"))