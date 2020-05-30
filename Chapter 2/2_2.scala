/* Implement isSorted, which checks whether an Array[A] is sorted according to a
given comparison function */

def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
  @annotation.tailrec
  def loop(n: Int): Boolean = 
    if (n >= as.length) true
    else if (!ordered(as(n-1), as(n))) false
    else loop(n+1)
  
  loop(1)
}

def orderedInt(i: Int, j: Int): Boolean = i <= j
assert(isSorted(Array(1,2,3,4,5), orderedInt))
assert(isSorted(Array(1,2,2,2,3), orderedInt))
assert(!isSorted(Array(1,2,4,3,5), orderedInt))
assert(!isSorted(Array(2,1,2,3,4), orderedInt))
assert(!isSorted(Array(1,2,3,5,4), orderedInt))