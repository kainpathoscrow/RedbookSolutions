// Implement the function tail for removing the first element of a List.

def tail[T](xs: List[T]): List[T] = xs match {
  case x :: tail => tail
  case Nil => Nil
}