package sample.hello

import akka.actor.{ActorSystem, Props}
import akka.pattern.{Backoff, BackoffSupervisor}

import scala.concurrent.duration._

object Main {

  def main(args: Array[String]): Unit = {
    val childProps = Props(classOf[HelloWorld])
    implicit val system = ActorSystem.create()
    val supervisor = BackoffSupervisor.props(
      Backoff.onStop(
        childProps   = childProps,
        childName    = "myEcho",
        minBackoff   = 3.seconds,
        maxBackoff   = 10.seconds,
        randomFactor = 0.2 // adds 20% "noise" to vary the intervals slightly
      ))
    system.actorOf(supervisor, name = "echoSupervisor")
   // akka.Main.main(Array(classOf[HelloWorld].getName))
  }
}
