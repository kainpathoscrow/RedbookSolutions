// Implement translate using runFree, and then use it to implement runConsole in a stack-safe way.

import cats._
import scala.io.StdIn.readLine

// Defs
sealed trait Free[F[_],A] {
  def map[B](f: A => B): Free[F, B] = flatMap(a => Return(f(a)))
  def flatMap[B](f: A => Free[F, B]): Free[F, B] = FlatMap(this, f)
}
case class Return[F[_],A](a: A) extends Free[F,A]
case class Suspend[F[_],A](s: F[A]) extends Free[F,A]
case class FlatMap[F[_],A,B](s: Free[F,A], f: A => Free[F,B]) extends Free[F,B]
def freeMonad[F[_]]: Monad[({type f[a] = Free[F,a]})#f] = new Monad[({type f[a] = Free[F,a]})#f] {
  def pure[A](a: A) = Return(a)
  def flatMap[A,B](fa: Free[F, A])(f: A => Free[F, B]) = fa.flatMap(f)  
  def tailRecM[A, B](a: A)(f: A => Playground.Free[F,Either[A,B]]): Playground.Free[F,B] = ??? // cats-required
}

@annotation.tailrec
def step[F[_], A](free: Free[F,A]): Free[F,A] = free match {
  case FlatMap(FlatMap(x,f), g) => step(x flatMap (a => f(a) flatMap g))
  case FlatMap(Return(x), f) => step(f(x))
  case _ => free
}
def runFree[F[_],G[_]:Monad,A](free: Free[F,A])(t: F ~> G): G[A] = step(free) match {
  case Return(a) => Monad[G].pure(a)
  case Suspend(r) => t(r)
  case FlatMap(Suspend(r),f) => Monad[G].flatMap(t(r))(a => runFree(f(a))(t))
  case _ => sys.error("Impossible; `step` eliminates these cases")
}
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

trait Translate[F[_], G[_]] { def apply[A](f: F[A]): G[A] }
type ~>[F[_], G[_]] = Translate[F,G]

sealed trait Console[A] {
  def toThunk: () => A
}
case object ReadLine extends Console[Option[String]] {
  def toThunk = () => run
  def run: Option[String] = try Some(readLine()) catch { case e: Exception => None }
}
case class PrintLine(line: String) extends Console[Unit] {
  def toThunk = () => println(line)
}
object Console {
  type ConsoleIO[A] = Free[Console, A]
  def readLn: ConsoleIO[Option[String]] = Suspend(ReadLine)
  def printLn(line: String): ConsoleIO[Unit] = Suspend(PrintLine(line))
}
val consoleToFunction0 = new (Console ~> Function0) { def apply[A](a: Console[A]) = a.toThunk }

// Solution
def translate[F[_],G[_],A](f: Free[F,A])(fg: F ~> G): Free[G,A] = {
  val translateFFreeG = new ~>[F, Free[G,?]] {
    def apply[A](a: F[A]): Free[G,A] = Suspend(fg(a))
  }
  implicit val freeM = freeMonad[G]
  runFree(f)(translateFFreeG)
}

def runConsole[A](a: Free[Console,A]): A = {
  val translated = translate[Console, Function0,A](a)(consoleToFunction0)
  runTrampoline(translated)
}

// Tests
val printOne: Free[Console, Int] = Return(1)
runConsole(printOne) // Should print 1