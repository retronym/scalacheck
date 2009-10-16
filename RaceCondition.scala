class Counter {
  import actors.Actor._

  private val server = actor {
    var n = 0
    loop { react {
      case 'stop => exit
      case 'get => reply(n)
      case m:Int => n = m
    } }
  }

  def get = (server !? 'get).asInstanceOf[Int]
  def set(n: Int) = server ! n
  def inc = set(get + 1) // race condition
  def reset = set(0)
  def close = server ! 'stop
}

import org.scalacheck._

object CounterSpecification extends Commands {

  val counter = new Counter

  case class State(n: Int)

  def initialState() = {
    counter.reset
    State(counter.get)
  }

  case object Inc extends Command {
    def run(s: State) = counter.inc
    def nextState(s: State) = State(s.n+1)
  }

  case object Get extends Command {
    def run(s: State) = counter.get
    def nextState(s: State) = s
    postConditions += {
      case (s0, s1, r:Int) => r == s1.n
    }
  }

  def genCommand(s: State) = Gen.oneOf(Inc, Get)
}
