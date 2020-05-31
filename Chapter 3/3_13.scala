// Can you write foldLeft in terms of foldRight? How about the other way around?

def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B = 
  as.foldRight((x: B) => x)((acc,cur) => x => cur(f(x,acc)))(z)

assert(foldLeft(Nil, "a")((a, b) => a+b) == "a")
assert(foldLeft(List(1,2,3), 0)((a, b) => a+b) == 6)
assert(foldLeft(List(1,2,3), 0)((a, b) => a-b) == -6) // (((0 - 1) - 2) - 3)

def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = 
  as.foldLeft((x: B) => x)((cur,acc) => x => cur(f(acc, x)))(z)

assert(foldRight(List[String](), "a")((a, b) => a+b) == "a")
assert(foldRight(List(1,2,3), 0)((a, b) => a+b) == 6)
assert(foldRight(List(1,2,3), 0)((a, b) => a-b) == 2) /// (0 - (1 - (2 - (3 - 0))))