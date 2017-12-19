package catsbook
final case class Box[A](value: A)
final case class Cat(name: String, age: Int, color: String)
trait Printable[A] { self ⇒
  def format(a:A):String
  def contramap[B](func: B => A): Printable[B] =
    new Printable[B] {
      def format(value: B): String =
        (func andThen self.format)(value)
    }
}

object Printable {

  implicit val stringPrintable: Printable[String] =
    (value: String) => "\"|" + value + "|\""

  implicit val booleanPrintable: Printable[Boolean] =
    (value: Boolean) => if (value) "yes" else "no"

  implicit def printBox[A:Printable]:Printable[Box[A]] =
    implicitly[Printable[A]].contramap(_.value)



   implicit val printableInt: Printable[Int] = (i: Int) => i.toString
  implicit val catPrinter:Printable[Cat] = (a: Cat) => {
    val age = implicitly[Printable[Int]].format(a.age)
    val name = implicitly[Printable[String]].format(a.name)
    val color = implicitly[Printable[String]].format(a.color)
    s"Cat: $name, color $color , age $age"
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
object CatsEq {
  import cats.Eq
  import cats.syntax.eq._
  import cats.instances.int._ // for Eq
  import cats.instances.string._ // for Eq

  implicit val catEqual: Eq[Cat] =
    Eq.instance[Cat] { (cat1, cat2) =>
      (cat1.name === cat2.name ) &&
        (cat1.age === cat2.age ) &&
        (cat1.color === cat2.color)
    }
}

object Test extends App {
  import Printable._
  import CatShow._
  import CatsEq._
  import cats.syntax.eq._
//  def print[A:Printable](a:A) = implicitly[Printable[A]].format(a)
//  println(print("asd"))print
//  println(print(1))
//
//  println(print(Cat("Toby", 1, "red")))
Box(1).print
Box("ggg").print

  Cat("Toby", 1, "red").print
  import cats.syntax.show._ // for show
  println(Cat("Toby", 1, "red").show)

  assert(
    Cat("Toby", 1, "red") === Cat("Toby", 1, "red")
  )
  println(Cat("Toby", 1, "red") =!= Cat("Toby", 1, "red"))
}