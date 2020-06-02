// Write a function that turns each value in a List[Double] into a String.

def doublesToString(xs: List[Double]): List[String] = xs.foldRight(List[String]())((cur, acc) => cur.toString :: acc)

assert(doublesToString(Nil) == Nil)
assert(doublesToString(List(1.0, 2.1, 3.5)) == List("1.0", "2.1", "3.5"))