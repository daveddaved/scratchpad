trait Fruit extends Product
case class Apple(a:Int) extends Fruit
val a = Apple(1)
val a1 = a.copy(a = 2)