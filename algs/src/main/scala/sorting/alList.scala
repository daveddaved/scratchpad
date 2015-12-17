package sorting

sealed trait List[+A]{
  def reverse[AA >: A](acc:List[AA] = Nil):List[AA] = this match {
    case Nil => acc
    case Cons(h,t) => t.reverse(Cons(h,acc))
  }
}
case object Nil extends List[Nothing]
case class Cons[A](head:A, tail:List[A]) extends List[A]

object testList extends App {
val xs:List[Int] = Cons(1, Cons(2, Cons(99, Cons(101,Nil))))

def search[A](a:A, xs:List[A], start:Int = -1):Int =
  xs match {
   case Nil =>  start
   case Cons(head, tail) if head == a => start + 1
   case Cons(head, tail) => search(a, tail, start + 1)
 }

  println(search(1,xs))
  println(xs.reverse())
  println(Cons(1,Nil).reverse())
  println(Nil.reverse())
}
