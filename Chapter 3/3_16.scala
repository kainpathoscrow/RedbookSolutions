// Write a function that transforms a list of integers by adding 1 to each element. (Reminder: this should be a pure function that returns a new List!)

def add1(xs: List[Int]): List[Int] = xs.foldRight(List[Int]())((cur, acc) => cur + 1 :: acc)

assert(add1(Nil) == Nil)
assert(add1(List(0,1,2)) == List(1,2,3))