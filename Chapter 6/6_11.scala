// Defs
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
        (nextA :: accList, nextS)
      }
    })
  
  def get[S]: State[S, S] = State(s => (s, s))
  def set[S](s: S): State[S, Unit] = State(_ => ((), s))
  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get
    _ <- set(f(s))
  } yield ()
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input
case class Machine(locked: Boolean, candies: Int, coins: Int)

// Solution
def processInput(m: Machine, i: Input): Machine = (m, i) match {
  case (Machine(_, 0, _), _) => m
  case (Machine(true, ca, co), Coin) => Machine(false, ca, co+1)
  case (Machine(false, ca, co), Turn) => Machine(true, ca-1, co)
  case _ => m
}

def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = for {
  steps <- State.sequence(inputs.map { i =>
    State.modify(m => processInput(m, i))
  })
  s <- State.get
} yield (s.coins, s.candies)

// Tests
val commands = List(Coin, Turn, Coin, Coin, Turn, Turn)
val initialMachine1 = Machine(true, 2, 0)
val initialMachine2 = Machine(false, 5, 0)

assert(simulateMachine(commands).run(initialMachine1)._1 == (2, 0))
assert(simulateMachine(commands).run(initialMachine2)._1 == (1, 3))