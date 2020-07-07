// Implement a listOf combinator that doesn’t accept an explicit size.
// It should return an SGen instead of a Gen. 
// The implementation should generate lists of the requested size.

// Defs
trait RNG {
  def nextInt: (Int, RNG)
}
case class State[S,+A](run: S => (A,S)) {
  def flatMap[B](f: A => State[S, B]): State[S, B] =
    State(s => {
      val (a, s1) = run(s)
      f(a).run(s1)
    })
  
  def map[B](f: A => B): State[S, B] = flatMap(a => State.unit(f(a)))
  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] = flatMap(a => sb.map(b => f(a, b)))
}
object State {
  def unit[S, A](a: A): State[S, A] = State(s => (a, s))
  
  def sequence[S, A](states: List[State[S, A]]): State[S, List[A]] = State(st => 
    states.reverse.foldRight((Nil: List[A], st)) {
      case (cur, (accList, accS)) => {
        val (nextA, nextS) = cur.run(accS)
        (nextA +: accList, nextS)
      }
    })    
}

case class Gen[+A](sample: State[RNG,A]) {
  def flatMap[B](f: A => Gen[B]): Gen[B] = Gen(sample.flatMap(f(_).sample))
  def listOfN(size: Int): Gen[List[A]] = Gen(State.sequence(List.fill(size)(sample)))
  def listOfN(size: Gen[Int]): Gen[List[A]] = size.flatMap(s => listOfN(s))
  def unsized: SGen[A] = SGen(i => this)
}

// Solution

case class SGen[+A](forSize: Int => Gen[A]) {
  def flatMap[B](f: A => SGen[B]): SGen[B] = SGen(i => {
    forSize(i).flatMap(a => f(a).forSize(i))
  })
  
  def listOfN(size: Int): SGen[List[A]] = SGen(i => {
    forSize(i).listOfN(size)
  })
    
  def listOfN(size: Gen[Int]): SGen[List[A]] = SGen(i => {
    forSize(i).listOfN(size)
  })
  
  def toGen(n: Int): Gen[A] = forSize(n)
}