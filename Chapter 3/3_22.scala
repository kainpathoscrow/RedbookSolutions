// Write a function that accepts two lists and constructs a new list by adding corresponding elements.

def zipWithSum(as: List[Int], bs: List[Int]): List[Int] = (as, bs) match {
  case (a::at, b::bt) => a+b :: zipWithSum(at, bt)
  case (Nil, _) => Nil
  case (_,Nil) => Nil
}

assert(zipWithSum(Nil,Nil) == Nil)
assert(zipWithSum(List(1,2), Nil) == Nil)
assert(zipWithSum(Nil, List(1,2)) == Nil)
assert(zipWithSum(List(1,2), List(3)) == List(4))
assert(zipWithSum(List(1), List(2,3)) == List(3))
assert(zipWithSum(List(1,2,3), List(4,5,6)) == List(5,7,9))