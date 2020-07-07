// Implement && and || for composing Prop values.

// Defs
trait RNG {
  def nextInt: (Int, RNG)
}

type FailedCase = String
type SuccessCount = Int
type TestCases = Int
sealed trait Result {
  def isFalsified: Boolean
}
case object Passed extends Result {
  def isFalsified = false
}
case class Falsified(failure: FailedCase, successes: SuccessCount) extends Result {
  def isFalsified = true
}


// Solution
case class Prop(run: (TestCases,RNG) => Result) {
  def &&(p: Prop): Prop = Prop((tcs,rng) => 
    run(tcs, rng) match {
      case Passed => p.run(tcs, rng)
      case f @ Falsified(_, _) => f
    }
  )
  
  def ||(p: Prop): Prop = Prop ((tc,rng) => 
    run(tc, rng) match {
      case pass @ Passed => pass
      case f @ Falsified(_, _) => p.run(tc, rng)
    }
  )
}
