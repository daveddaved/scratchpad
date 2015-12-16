package scratchpad.ch8testing

import java.util.Random

import scratchpad.ch6state.RNG.Simple
import scratchpad.ch8testing.Prop.{SuccessCount, FailedCase}
import scratchpad.ch6state.{State,RNG}

trait Prop {
  def check: Either[(FailedCase, SuccessCount), SuccessCount]
  def &&(p:Prop):Prop = new Prop {
    override def check: Either[(FailedCase, SuccessCount), SuccessCount] =
      Prop.this.check match {
        case l@Left(_) => l
        case Right(sc) => p.check
    }
  }
}

object Prop {
  type FailedCase = String
  type SuccessCount = Int
}

case class Gen[A](sample: State[RNG,A]){
  def choose(start: Int, stopExclusive : Int):Gen[Int] =
    Gen(State(RNG.nonNegativeInt).map(i => start + i% (stopExclusive - start)))
}

object testGens extends App {
  val rng = new RNG.Simple(scala.util.Random.nextLong())
  val g = Gen[Int](State(RNG.int))
  println(g.choose(-10000,10000).sample.run(rng))
}