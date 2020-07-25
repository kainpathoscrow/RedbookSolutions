// Implement a specialized tail-recursive interpreter, runTrampoline, for running a Free[Function0,A].

// Defs
sealed trait Free[F[_],A] {
  def map[B](f: A => B): Free[F, B] = flatMap(a => Return(f(a)))
  def flatMap[B](f: A => Free[F, B]): Free[F, B] = FlatMap(this, f)
}
case class Return[F[_],A](a: A) extends Free[F,A]
case class Suspend[F[_],A](s: F[A]) extends Free[F,A]
case class FlatMap[F[_],A,B](s: Free[F,A], f: A => Free[F,B]) extends Free[F,B]

// Solution
@annotation.tailrec
def runTrampoline[A](a: Free[Function0,A]): A = a match {
  case Return(a) => a
  case Suspend(s) => s()
  case FlatMap(s, f) => s match {
    case Return(a1) => runTrampoline(f(a1))
    case Suspend(r1) => runTrampoline(f(r1()))
    case FlatMap(s1, f1) => runTrampoline(s1.flatMap(a => f1(a) flatMap f))
  } 
}

// Tests
type TailRec[A] = Free[Function0,A]
val returnOne: TailRec[Int] = Return(1)
val suspendOne: TailRec[Int] = Suspend(() => 1)
val flatMapToString: TailRec[String] = FlatMap(returnOne, (i: Int) => Return(i.toString))

assert(runTrampoline(returnOne) == 1)
assert(runTrampoline(suspendOne) == 1)
assert(runTrampoline(flatMapToString) == "1")