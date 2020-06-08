// Implement versions of map, flatMap, orElse, and map2 on Either that operate on the Right value.

sealed trait Either[+E, +A] {
  def map[B](f: A => B): Either[E, B] = this match {
    case l @ Left(_) => l
    case Right(r) => Right(f(r)) 
  }
  
  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case l @ Left(_) => l
    case Right(r) => f(r)
  }
  
  def orElse[EE >: E,B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
    case Left(_) => b
    case Right(_) => this
  }
  
  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = this match {
    case l @ Left(_) => l
    case Right(a) => b match {
      case l @ Left(_) => l
      case Right(br) => Right(f(a, br))
    }
  }
}

case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]


assert((Left("Error"): Either[String, Int]).map(_ + 1) == Left("Error"))
assert((Right(0): Either[String, Int]).map(_ + 1) == Right(1))

assert((Left("Error"): Either[String, Int]).flatMap(l => Left("Error1")) == Left("Error"))
assert((Left("Error"): Either[String, Int]).flatMap(r => Right(r + 1)) == Left("Error"))
assert((Right(0): Either[String, Int]).flatMap(l => Left("Error")) == Left("Error"))
assert((Right(0): Either[String, Int]).flatMap(r => Right(r + 1)) == Right(1))

assert((Left("Error"): Either[String, Int]).orElse(Left("Error1")) == Left("Error1"))
assert((Left("Error"): Either[String, Int]).orElse(Right(0)) == Right(0))
assert((Right(0): Either[String, Int]).orElse(Left("Error")) == Right(0))
assert((Right(0): Either[String, Int]).orElse(Right(1)) == Right(0))

assert((Left("Error"): Either[String, Int]).map2(Left("Error1"))(_ + _) == Left("Error"))
assert((Left("Error"): Either[String, Int]).map2(Right(0))(_ + _) == Left("Error"))
assert((Right(1): Either[String, Int]).map2(Left("Error"))(_ + _) == Left("Error"))
assert((Right(1): Either[String, Int]).map2(Right(2))(_ + _) == Right(3))