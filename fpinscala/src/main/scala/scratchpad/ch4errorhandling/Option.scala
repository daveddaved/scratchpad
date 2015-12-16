package scratchpad.ch4errorhandling

import scratchpad.ch3datastructs.Lists._


import scala.{Either => _, None => _, Option => _, Some => _}

sealed trait Option[+A] {
  /**
  It’s fine to use pattern matching, though you should be able to implement all
the functions besides map and getOrElse without resorting to pattern matching.
 For map and flatMap , the type signature should be enough to determine the
implementation.
 getOrElse returns the result inside the Some case of the Option , or if the Option
is None , returns the given default value.
 orElse returns the first Option if it’s defined; otherwise, it returns the second
Option .
   */

  def map[B](f: A => B): Option[B] = this match {
    case Some(a) => Some(f(a))
    case _ => None
  }
  def flatMap[B](f: A => Option[B]): Option[B] = map (f) getOrElse(None)

  def getOrElse[B >: A](default: => B): B = this match {
    case Some(v) => v
    case _ => default
  }
  def orElse[B >: A](ob: => Option[B]): Option[B] =
    map{v => Some(v)} getOrElse ob

  def filter(f: A => Boolean): Option[A] =
    flatMap{ v => if(f(v)) Some(v) else None }


}

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {
  def lift[A,B](f: A => B): Option[A] => Option[B] = _ map f
  val absO: Option[Double] => Option[Double] = lift(Math.abs)
  /**
   * Write a generic function map2 that combines two Option values using a binary func-
   * tion. If either Option value is None , then the return value is too. Here is its signature:
   */
  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
      a.flatMap(aa => b.map(bb => f(aa,bb)))

  /**
   * Write a function sequence that combines a list of Option s into one Option containing
   * a list of all the Some values in the original   If the original list contains None even
   *  once, the result of the function should be None ; otherwise the result should be Some
   * with a list of all the values. Here is its signature: 3
   */

  def sequence[A](a: List[Option[A]]): Option[List[A]] =
    a match {
      case Nil => Some(Nil)
      case Cons(h,t) => map2(h, sequence(t)) { Cons(_,_)  }
    }

  /**
   * Implement this function. It’s straightforward to do using map and sequence , but try
   * for a more efficient implementation that only looks at the list once. In fact, imple-
   * ment sequence in terms of traverse .
   */
  def traverse[A, B](a:  List[A])(f: A => Option[B]): Option[ List[B]] =
    a match {
      case  Nil => Some( Nil)
      case Cons(h,t) => map2(f(h), traverse(t)(f)){ Cons(_,_) }
    }
  def sequence2[A](a:  List[Option[A]]): Option[ List[A]] =
    traverse(a)(x => x)
}

object optionTest extends App {
//  Implement the variance function in terms of flatMap . If the mean of a sequence is m ,
//  the variance is the mean of math.pow(x - m, 2) for each element x in the sequence.
//  See the definition of variance on Wikipedia (http://mng.bz/0Qsr).
  def mean(ys:Seq[Double]):Option[Double] = if(ys.isEmpty) None else Some(ys.sum / ys.length)
  def variance(xs: Seq[Double]): Option[Double] =
   mean(xs).flatMap { m => mean(xs.map(x => math.pow(x - m,2 ))) }
 // SD ???

  println(Option.absO(Some(-2)))

println(variance(Vector(122,2,13,4,4,5,6).map(_.toDouble)))
  val xs: List[Option[Int]] = Cons(Some(1),Cons(Some(2), Cons(Some(3),Cons(None, Nil))))
  val ys: List[Option[Int]] = Cons(Some(1),Cons(None, Cons(Some(3),Cons(Some(9), Nil))))
  println(   List.map(xs) { o => o.map(_.toString  + " string ") } )
  println(Option.sequence( List(Some(1))))
  println(Option.sequence( Nil))
  println(Option.sequence(xs))
  println(Option.sequence(List.init(xs)))
  println(Option.sequence(ys))
  println(Option.sequence2(ys))
  println(Option.sequence(List.filter(ys) (_!=None)))
  println(Option.sequence2(List.filter(ys) (_!=None)))

}
