package scratchpad.ch9parsing
import scratchpad.ch8testing._
import Prop._
import Gen._
import language.higherKinds
import language.implicitConversions

import java.util.regex._
import scala.util.matching.Regex


trait Parsers[ParseError, Parser[+_]] { self =>
  def run[A](p: Parser[A])(input: String): Either[ParseError,A]
//  def char(c: Char): Parser[Char]
  def or[A](s1: Parser[A], s2: => Parser[A]): Parser[A]
//  run(or(string("abra"),string("cadabra")))("abra") == Right("abra")
//  run(or(string("abra"),string("cadabra")))("cadabra") == Right("cadabra")
  implicit def string(s: String): Parser[String]

  implicit def operators[A](p: Parser[A]):ParserOps[A] = ParserOps[A](p)
  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]):
      ParserOps[String] = ParserOps(f(a))

  implicit def char(c:Char):Parser[Char] = map(string(c.toString))(_.charAt(0))

  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] =
    (0 to n).
      foldRight(succeed(List[A]()))((count,xs) =>
        map2(xs, listOfN(count,p))(_++_))
//    if(n <= 0) succeed(Nil)
//    else map2(p, listOfN(n - 1, p))(_ :: _)
    // For instance, how would we recognize three repetitions of our "abra" |
    // "cadabra" parser? Once again, let’s add a combinator for it:
    // def many[A](p: Parser[A]): Parser[List[A]]
  def flatMap[A,B](p: Parser[A])(f: A => Parser[B]): Parser[B]
  def succeed[A](a: A): Parser[A] = string("...") map( _=> a )
  def slice[A](p: Parser[A]): Parser[String]
  def map[A,B](a: Parser[A])(f: A => B): Parser[B]
  def product[A,B](p: Parser[A], p2: => Parser[B]): Parser[(A,B)]
  def map2[A,B,C](p: Parser[A], p2: => Parser[B])(f: (A,B) => C): Parser[C] =
    (p ** p2) map f.tupled

  def map22[A,B,C](p: Parser[A], p2: => Parser[B])(f: (A,B) => C): Parser[C] =
    for {
      p11 <- p
      p22 <- p2
    } yield f(p11,p22)

  def product22[A,B](p: Parser[A], p2: => Parser[B]): Parser[(A,B)] =
    for {
      p11 <- p
      p22 <- p2
    } yield (p11,p22)


  def many[A](p: => Parser[A]): Parser[List[A]] =
    map2(p, many(p))(_ :: _) | succeed(List())

  def many1[A](p: => Parser[A]): Parser[List[A]] =
    map2(p, many(p))(_ :: _)

  implicit def regex(r: Regex): Parser[String]


  case class ParserOps[A](p: Parser[A]) {
    def |[B>:A](p2: Parser[B]): Parser[B] = self.or(p,p2)
    def or[B>:A](p2: => Parser[B]): Parser[B] = self.or(p,p2)
  //  def many(p: Parser[A]): Parser[List[A]]

    def map[B](f: A => B): Parser[B] = self.map(p)(f)

    def slice: Parser[String] = self.slice(p)

    def flatMap[B](f: A => Parser[B]): Parser[B] =
      self.flatMap(p)(f)

    def product[B](p2: Parser[B]): Parser[(A,B)] = self.product(p,p2)
    def **[B](p2: Parser[B]): Parser[(A,B)] = product(p2)

  }
  object Laws {
    def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop =
      forAll(in)(s => run(p1)(s) == run(p2)(s))

    def mapLaw[A](p: Parser[A])(in: Gen[String]): Prop =
      equal(p, p.map(a => a))(in)

  }

}
