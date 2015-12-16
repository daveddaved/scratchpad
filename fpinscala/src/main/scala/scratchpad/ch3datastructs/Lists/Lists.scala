package scratchpad.ch3datastructs.Lists

import scratchpad.ch3datastructs

trait List[+T]
case object Nil extends List[Nothing]
case class Cons[+T](head:T, tail:List[T]) extends List[T]

object List { // `List` companion object. Contains functions for creating and working with lists.

  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else ch3datastructs.Lists.Cons(as.head, apply(as.tail: _*))

  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => ch3datastructs.Lists.Cons(h, append(t, a2))
    }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar

  /**
   * Implement the function tail for removing the first element of a List.
   * Note that the function takes constant time. What are different choices you could make in your implementation
   * if the List is Nil? We’ll return to this question in the next chapter.
   */
  def tail[A](l: List[A]): List[A] = l match {
    case Nil => sys.error("Tail of empty list")
    case Cons(h,t) => t
  }

  /**
   * Using the same idea, implement the function setHead for
   * replacing the first element of a List with a different value.
   */

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil => sys.error("empty list")
    case Cons(_,t) => ch3datastructs.Lists.Cons(h,t)
  }

  /**
   * Implement dropWhile, which removes elements from the List prefix as long as they match a predicate.
   */
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(h,t) if f(h) => dropWhile(t, f)
  }

  /**
   * Generalize tail to the function drop, which removes the first n elements from a list.
   * Note that this function takes time proportional only to the number of elements being
   * dropped—we don’t need to make a copy of the entire List.
   */

  def drop[A](l: List[A], n: Int): List[A] =
  if(n <= 0) l else
    l match {
    case Nil => Nil
    case Cons(_,t) => drop(t, n -1)
  }

  /**
   * Not everything works out so nicely. Implement a function, init, that returns a List consisting of all but the last element of a List.
   * So, given List(1,2,3,4), init will return List(1,2,3). Why can’t this function be implemented in constant time like tail?
   */
  def init[A](l: List[A]): List[A] = l match {
    case Nil => sys.error("empty list")
    case Cons(_,Nil) => Nil
    case Cons(h,t) => ch3datastructs.Lists.Cons(h, init(t))
  }

  def length[A](l: List[A]): Int = foldRight(l, 0)((_,zp) => zp + 1)

  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(h,t) => foldLeft(t, f(z,h))(f)
  }
  def sumViaFoldLeft(l:List[Int]):Int = foldLeft(l,0) {_ + _}
  def productViaFoldLeft(l:List[Int]):Int = foldLeft(l,1) {_ * _}
  def reverse[A](xs:List[A]):List[A] = foldLeft(xs, List[A]()) { (acc,h) => ch3datastructs.Lists.Cons(h,acc)  }

  def frViaFl[A,B](as: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(reverse(as),z) { (b,a) => f(a,b) }

  def append2[A](xs:List[A], ys:List[A]) = foldRight(xs,ys) {  ch3datastructs.Lists.Cons(_,_)  }
  def append3[A](xs:List[A], ys:List[A]) = foldLeft(reverse(xs),ys) {(r,l) =>  ch3datastructs.Lists.Cons(l,r)}

  /**
   * Write a function that transforms a list of integers by adding 1 to each element. (Reminder: this should be a pure function that returns a new List!)
   */
  def add1(xs:List[Int]):List[Int] = foldRight(xs, List[Int]()) { (x,acc) => ch3datastructs.Lists.Cons(x + 1, acc)  }

  def dblToString(xs:List[Double]):List[String] = foldRight(xs, List[String]()) { (x,acc) => ch3datastructs.Lists.Cons(x.toString, acc)  }

  def map[A,B](l: List[A])(f: A => B): List[B] = foldRight(l, List[B]()) { (x,acc) => ch3datastructs.Lists.Cons(f(x), acc) }

  /**
   * Write a function filter that removes elements from a list unless they satisfy a given predicate. Use it to remove all odd numbers from a List[Int].
   */

  def filter[A](as: List[A])(f: A => Boolean): List[A] = foldRight(as, List[A]()) {(a, acc) =>
    if (f(a)) ch3datastructs.Lists.Cons(a, acc) else acc
  }

  /**
   * Write a function flatMap that works like map except that the function given will return a list instead of a single result,
   * and that list should be inserted into the final resulting list.
   */

  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] = foldRight(as, List[B]()) { (a, acc) => append(f(a), acc) }

  /**
   * Use flatMap to implement filter.
   */
  def filter2[A](as: List[A])(f: A => Boolean): List[A] = flatMap(as) { a => if(f(a)) List(a) else Nil }

  /**
   * Write a function that accepts two lists and constructs a new list by adding correspond- ing elements.
   * For example, List(1,2,3) and List(4,5,6) become List(5,7,9)
   */
  def add2List(xs:List[Int], ys:List[Int]):List[Int] = (xs,ys) match {
    case (_,Nil) | (Nil, _) => Nil
    case (Cons(h1,t1),Cons(h2,t2)) => ch3datastructs.Lists.Cons(h1 + h2, add2List(t1,t2) )
  }

  /**
   * Generalize the function you just wrote so that it’s not specific to integers or addition. Name your generalized function zipWith.
   */
  def zipWith[A,B,C](xs:List[A], ys:List[B])(f:(A,B) => C):List[C] = (xs,ys) match {
    case (_,Nil) | (Nil, _) => Nil
    case (Cons(h1,t1),Cons(h2,t2)) => Cons(f(h1,h2), zipWith(t1,t2)(f) )
  }

  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = (sup,sub) match {
      case(Nil,s) => s == Nil
      case(Cons(_,_),Nil) => true
      case (Cons(h1,t1), Cons(h2,t2)) =>
        if (startsWith(sup,sub)) true
        else hasSubsequence(t1, sub)
  }
  def startsWith[A](sup1: List[A], sub1: List[A]): Boolean = (sup1,sub1) match {
    case(Nil,s) => s == Nil
    case(Cons(_,_),Nil) => true
    case (Cons(h1,t1), Cons(h2,t2)) => if(h1 == h2) startsWith(t1,t2) else false
  }
}


object ListsTest extends App {
import List._

    val l1 = ch3datastructs.Lists.Cons(1,ch3datastructs.Lists.Cons(2,Nil))
    val l2 = ch3datastructs.Lists.Cons(3,ch3datastructs.Lists.Cons(4,Nil))
    val l4 = ch3datastructs.Lists.Cons(2,ch3datastructs.Lists.Cons(3,ch3datastructs.Lists.Cons(4,Nil)))

    val l3 = ch3datastructs.Lists.Cons(1.0, ch3datastructs.Lists.Cons(2.0,Nil))

  println(List.append2(l1,l2))
  println(List.append3(l1,l2))

  println(List.add1(l1))
  println( List.map(l2) {_ + 10} )
  println( List.filter(l2) {_ == 3} )
  println( List.flatMap(l2) { x=> List(x,x)} )
  println( hasSubsequence(append(l1,l2), l1))
  println( hasSubsequence(append(l1,l2), l2))
  println( hasSubsequence(append(l1,l2), map(l4)(_+10)))
}
