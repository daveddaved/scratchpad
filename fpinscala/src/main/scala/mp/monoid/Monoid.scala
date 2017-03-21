package mp.monoid

import mp.semigroup.Semigroup

trait Monoid[A] extends Semigroup[A] {
  def zero:A
}

object Monoid {
  def apply[A:Monoid]:Monoid[A] = implicitly

  implicit object Integers extends Monoid[Int] {
    val zero = 0
    override def add(x: Int, y: Int):Int = x + y
  }

  implicit object Strings extends Monoid[String] {
    val zero = ""
    override def add(x: String, y: String):String = x + y
  }
}