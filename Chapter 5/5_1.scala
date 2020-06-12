// Write a function to convert a Stream to a List, which will force its evaluation and let you look at it in the REPL.
// You can convert to the regular List type in the standard library.
// You can place this and other functions that operate on a Stream inside the Stream trait.

def toList[A](stream: Stream[A]): List[A] = stream match {
  case Stream.Empty => List[A]()
  case head #:: tail => head :: toList(tail)
}


assert(toList(Stream.empty) == Nil)
assert(toList(1 #:: 2 #:: Stream.empty) == List(1,2))