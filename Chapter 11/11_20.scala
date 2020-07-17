// Give a monad instance for the following type

import cats._
import cats.data._
import scala.annotation.tailrec

// Solution

def readerMonad[R] = new Monad[({type f[x] = Reader[R,x]})#f] {
  def pure[A](a: A): Reader[R,A] = Reader.apply(_ => a)
  def flatMap[A,B](st: Reader[R,A])(f: A => Reader[R,B]): Reader[R,B] =
    Reader.apply(r => f(st.run(r)).run(r))

  def tailRecM[A, B](a: A)(f: A => Reader[R, Either[A, B]]): Reader[R, B] = {
    @tailrec
    def loop(thisA: A)(r: R): B =
      f(thisA)(r) match {
        case Right(b)    => b
        case Left(nextA) => loop(nextA)(r)
    }
    Reader.apply(loop(a))
  }
}
