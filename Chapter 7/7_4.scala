// Using lazyUnit, write a function to convert any function A => B to one that evaluates its result asynchronously.

// Defs
trait Par[A]

def fork[A](a: => Par[A]): Par[A] = ???
def unit[A](a: A): Par[A] = ???
def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

// Solution
def asyncF[A,B](f: A => B): A => Par[B] = a => lazyUnit(f(a)) 
