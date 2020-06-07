// Write a function sequence that combines a list of Options into one Option containing a list of all the Some values in the original list. 
// If the original list contains None even once, the result of the function should be None; otherwise the result should be Some with a list of all the values. 

def sequence[A](a: List[Option[A]]): Option[List[A]] = a match {
  case xOpt :: xsOpt => for {
    x <- xOpt
    xs <- sequence(xsOpt)
  } yield x :: xs
  case Nil => Some(Nil)
}

assert(sequence(Nil) == Some(Nil))
assert(sequence(List(None, Some(1))) == None)
assert(sequence(List(Some(1), None)) == None)
assert(sequence(List(Some(1), Some(2))) == Some(List(1, 2)))