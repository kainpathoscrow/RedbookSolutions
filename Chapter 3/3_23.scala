// Generalize the function you just wrote so that it’s not specific to integers or addition.

def zipWith[A,B,T](as: List[A], bs: List[B])(f: (A, B) => T): List[T] = (as, bs) match {
  case (a::at, b::bt) => f(a,b) :: zipWith(at, bt)(f)
  case (Nil, _) => Nil
  case (_,Nil) => Nil
}

assert(zipWith(List(1,2), List("a", "b"))((num, str) => str * num) == List("a", "bb"))