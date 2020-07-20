// Implement sequence over a Map rather than a List: 

import cats._
import cats.implicits._

// Solution
def sequenceMap[K,V,F[_]:Applicative](ofa: Map[K,F[V]]): F[Map[K,V]] = {
  val fa = implicitly[Applicative[F]]
  ofa.foldLeft(fa.pure(Map.empty[K, V])) ((accF, cur) => 
    fa.map2(accF, cur._2)((acc, v) => acc + (cur._1 -> v))
  )
}

// Tests
assert(sequenceMap(Map(1 -> Some("1"), 2 -> None)) == None)
assert(sequenceMap(Map[Int, Option[String]](1 -> Some("1"), 2 -> Some("2"))) == Some(Map(1 -> "1", 2 -> "2")))