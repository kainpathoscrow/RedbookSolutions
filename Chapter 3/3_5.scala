// Implement dropWhile, which removes elements from the List prefix as long as they match a predicate.

def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
  case x :: tail => if (f(x)) dropWhile(tail, f) else l
  case Nil => Nil
}