// Par.map2 is a new higher-order function for combining the result of two parallel computations. 
// What is its signature? Give the most general signature possible (don’t assume it works only for Int).

// Defs
trait Par[A]

// Solution
def map2[A,B,C](pa: Par[A], pb: Par[B])(f: (A, B) => C): Par[C] = ???