// Write a function fibs that generates the infinite stream of Fibonacci numbers: 0, 1, 1, 2, 3, 5, 8, and so on.

def fibs: Stream[Int] = {
  def step(prev2: Int, prev: Int): Stream[Int] = prev2 #:: prev #:: step(prev2+prev, prev+(prev2+prev))
    
  step(0, 1)
}

assert(fibs.take(6) == 0 #:: 1 #:: 1 #:: 2 #:: 3 #:: 5 #:: Stream.empty)