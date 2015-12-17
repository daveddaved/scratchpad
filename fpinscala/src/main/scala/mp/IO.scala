package mp
import mp.monads._

class State {

}
case class Player(name:String, score:Int)
object Game1 {
  def contest1(p1:Player,p2:Player):Unit = {
    if(p1.score < p2.score)
      println(s"${p1.name} is the winner")
    else if(p2.score > p1.score)
      println(s"${p2.name} is the winner")
    else
      println("It is a draw")
  }
  def winner(p1:Player,p2:Player):Option[Player] = {
    if (p1.score > p2.score) Some(p1)
    else if (p2.score > p1.score) Some(p2)
    else None
  }

  def gameMsg(po:Option[Player]):String =
    po map {case Player(name,_) => s"$name is the winner"} getOrElse "It is a draw"

  def contest2(p1:Player,p2:Player):Unit = {
    winner(p1,p2) match {
      case Some(Player(name,_)) => println(s"$name is the winner")
      case _ => println("It is a draw")
    }
  }
  def contest3(p1:Player,p2:Player):Unit =
    println(gameMsg(winner(p1,p2)))

//
//  def converter: Unit = {
//    println("Enter a temperature in degrees Fahrenheit: ")
//    val d = scala.io.StdIn.readLine.toDouble
//    println(fahrenheitToCelsius(d))
//  }
  def fahrenheitToCelsius(f: Double): Double =
    (f - 32) * 5.0/9.0

  def converter: IO[Unit] =
    for {
    _ <- PrintLine("Enter a temperature in degrees Fahrenheit: ")
    d <- ReadLine.map(_.toDouble)
    _ <- PrintLine(fahrenheitToCelsius(d).toString)
  } yield ()

  def ReadLine:IO[String] = IO { scala.io.StdIn.readLine }
  def PrintLine(msg:String):IO[Unit] = IO {  println(msg) }

  def contest(p1:Player,p2:Player):IO[Unit] =
    PrintLine(gameMsg(winner(p1,p2)))
}

sealed trait IO[A] { self =>
  def run:A

  def map[B](f:A =>B):IO[B] =
    new IO[B] {def run = f(self.run)}

  def flatMap[B](f:A => IO[B]):IO[B] = new IO[B] {
     def run = f(self.run).run
  }
  def ++(io:IO[A]):IO[A] = new IO[A] {
    def run = {self.run ; io.run}
  }
}

object IO extends Monad[IO] {
  def unit[A](a: => A):IO[A] = new IO[A]{def run = a }
  def pure[A](a:A):IO[A] = unit(a)
  def flatMap[A,B](fa:IO[A])(f:A => IO[B]):IO[B] = fa flatMap f
  def apply[A](a: => A):IO[A] = unit(a)
  def empty:IO[Unit] = new IO[Unit] {def run =()}
}
//case class Return[A](a: A) extends IO[A]
//case class Suspend[A](resume: () => A) extends IO[A]
//case class FlatMap[A,B](sub: IO[A], k: A => IO[B]) extends IO[B]
//

object Game extends App {
  import Game1._
  val p1 = Player("First", 10)
  val p2 = Player("Second", 11)

  contest(p1,p2).run
  Game1.contest2(p1,p2)
  Game1.converter.run
}
