// Implement the variance function in terms of flatMap.
// If the mean of a sequence is m, the variance is the mean of math.pow(x - m, 2) for each element x in the sequence.

def mean(xs: Seq[Double]): Option[Double] = xs match {
  case Nil => None
  case _ => Some(xs.sum / xs.length)
}

def variance(xs: Seq[Double]): Option[Double] = {
  val xsMean = mean(xs)
  xsMean.flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))
}

assert(mean(Nil) == None)
assert(mean(List(1.0, 2.0)) == Some(1.5))