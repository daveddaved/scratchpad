package mp.semigroup

trait Semigroup[A] {
  def add(x:A, y:A):A
}

object Semigroup {

  def apply[A:Semigroup]:Semigroup[A] = implicitly

  implicit object Integers extends Semigroup[Int] {
    override def add(x: Int, y: Int):Int = x + y
  }

  implicit object Strings extends Semigroup[String] {
    override def add(x: String, y: String):String = x + y
  }
}