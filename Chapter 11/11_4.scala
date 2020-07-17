// Implement replicateM.

import cats._
import cats.implicits._

// Defs
def sequence[A, M[_]](mas: List[M[A]])(implicit m: Monad[M]): M[List[A]] =
  mas.foldLeft(m.pure(Nil: List[A]))((acc, cur) => m.flatMap(acc)(accV => m.map(cur)(curV => accV :+ curV)))

// Solution
def replicateM[A, F[_]: Monad](n: Int, ma: F[A]): F[List[A]] = sequence(List.fill(n)(ma))

// Tests 
assert(replicateM(3, Some(1): Option[Int]) == Some(List(1,1,1)))