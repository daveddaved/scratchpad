package catsbook

import cats.syntax

final case class Cat(name: String, age: Int, color: String)
trait Printable[A] {
  def format(a:A):String
}

object Printable {
  implicit val printableString: Printable[String] = new Printable[String] {
    override def format(a: String): String = a
  }
   implicit val printableInt: Printable[Int] = new Printable[Int] {
    override def format(i: Int): String = i.toString
  }
  implicit val catPrinter:Printable[Cat] = new Printable[Cat] {
    override def format(a: Cat): String = {
      val age = implicitly[Printable[Int]].format(a.age)
      val name = implicitly[Printable[String]].format(a.name)
      val color = implicitly[Printable[String]].format(a.color)
      s"Cat: $name, color $color , age $age"
    }
  }
  implicit class PrintableOps[A](a:A) {
    def format(implicit p:Printable[A]):String = p.format(a)
    def print(implicit p:Printable[A]):Unit = println(p.format(a))
  }
}

object CatShow {
  import cats.Show
  import cats.instances.int._ // for Show
  import cats.instances.string._ // for Show
  import cats.syntax.show._ // for show

  implicit val catShow = Show.show[Cat] { cat =>
    val name = cat.name.show
    val age = cat.age.show
    val color = cat.color.show
    s"$name is a $age year-old $color cat."
  }

}

object Test extends App {
  import Printable._
  import CatShow._
//  def print[A:Printable](a:A) = implicitly[Printable[A]].format(a)
//  println(print("asd"))print
//  println(print(1))
//
//  println(print(Cat("Toby", 1, "red")))

  Cat("Toby", 1, "red").print
  import cats.syntax.show._ // for show
  println(Cat("Toby", 1, "red").show)
}