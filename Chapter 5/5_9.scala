// Write a function that generates an infinite stream of integers, starting from n, then n + 1, n + 2, and so on

def from(n: Int): Stream[Int] = n #:: from(n+1)

assert(from(0).take(3) == 0 #:: 1 #:: 2 #:: Stream.empty)