// Implement all of the preceding functions on Option.

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case Some(a) => Some(f(a))
    case None => None
  }
  
  def flatMap[B](f: A => Option[B]): Option[B] = this match {
    case Some(a) => f(a)
    case None => None
  }
  
  def getOrElse[B >: A](default: => B): B = this match {
    case Some(a) => a
    case None => default
  }
  
  def orElse[B >: A](ob: => Option[B]): Option[B] = this match {
    case some @ Some(a) => some
    case None => ob
  }
    
  def filter(f: A => Boolean): Option[A] = this match{
    case some @ Some(a) => if (f(a)) some else None 
    case None => None
  }
}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

assert((None: Option[Int]).map(_ + 1) == None)
assert(Some(0).map(_ + 1) == Some(1))

assert((None: Option[Int]).flatMap(v => Some(v + 1)) == None)
assert(Some(0).flatMap(v => Some(v + 1)) == Some(1))
assert(Some(0).flatMap(v => None) == None)

assert((None: Option[Int]).getOrElse(1) == 1)
assert(Some(0).getOrElse(1) == 0)

assert((None: Option[Int]).orElse(Some(1)) == Some(1))
assert(Some(0).orElse(Some(1)) == Some(0))

assert((None: Option[Int]).filter(_ > 0) == None)
assert(Some(0).filter(_ > 0) == None)
assert(Some(0).filter(_ <= 0) == Some(0))