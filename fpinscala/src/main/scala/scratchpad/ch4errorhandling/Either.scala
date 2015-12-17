package scratchpad.ch4errorhandling

import scratchpad.ch3datastructs.Lists._

import scala.{Either => _, Left => _, None => _, Option => _, Right => _, Some => _}

sealed trait Either[+E, +A] {
  def map[B](f: A => B): Either[E, B] = this match {
    case Right(x) => Right(f(x))
    case Left(y) => Left(y)
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]) : Either[EE, B] = this match {
      case Right(a) => f(a)
      case Left(b) => Left(b)
    }

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
      case Right(_) => this
      case Left(_) => b
    }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
   // flatMap(a => b.map(bb => f(a, bb)))
  for {aa <- this; bb <-b } yield f(aa,bb)

  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] =
    traverse(es)(identity)

  def traverse[E, A, B](as:  List[A])(f: A => Either[E, B]): Either[E,  List[B]] = as match {
    case  Nil => Right(Nil)
    case Cons(h, t) => f(h).map2(traverse(t)(f)) {  Cons(_, _) }
  }
}
  case class Left[+E](value: E) extends Either[E, Nothing]
  case class Right[+A](value: A) extends Either[Nothing, A]
/**
* Implement sequence and traverse for Either . These should return the first error
* that’s encountered, if there is one.
 */
