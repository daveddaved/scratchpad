package sample.hello

import java.time.Instant

import akka.actor.Actor

object Greeter {
  case object Greet
  case object Done
}

class Greeter extends Actor {
  def receive = {
    case Greeter.Greet =>
      println(Instant.now() +  " Hello World!")
      sender() ! Greeter.Done
  }
}
case class testAC(k:Int, v:String)
object t {
}
