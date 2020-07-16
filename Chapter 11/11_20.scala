// Give a monad instance for the following type

import cats._

// Defs
case class Reader[R, A](run: R => A)

// Solution
object Reader {
  def readerMonad[R] = new Monad[({type f[x] = Reader[R,x]})#f] {
    def pure[A](a: A): Reader[R,A] = Reader(_ => a)
    def flatMap[A,B](st: Reader[R,A])(f: A => Reader[R,B]): Reader[R,B] =
      Reader(r => f(st.run(r)).run(r))
    
    def tailRecM[A, B](a: A)(f: A => Reader[R, Either[A, B]]): Reader[R, B] = {
      flatMap(f(a)){
        case Left(nextA) => tailRecM(nextA)(f)
        case Right(b) => pure(b)
      }
    }
  }
}