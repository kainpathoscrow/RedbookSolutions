// Using the same idea, implement the function setHead for replacing the first element of a List with a different value. 

def setHead[T](xs: List[T], newValue: T): List[T] = xs match {
  case x :: tail => newValue :: tail
  case Nil => Nil
}
