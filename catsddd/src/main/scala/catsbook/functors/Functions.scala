package catsbook.functors

object Functions extends App {

  import cats.instances.function._ // for Functor
  import cats.syntax.functor._ // for map

  val func1: Int => Double =
    (x: Int) => x.toDouble

  val func2: Double => Double =
    (y: Double) => y * 2
  (func1 map func2)(1) // composition using map
  // res7: Double = 2.0
  (func1 andThen func2)(1) // composition using andThen
  // res8: Double = 2.0
  func2(func1(1)) // composition written out by hand
  // res9: Double = 2.0

  val func =
    ((x: Int) => x.toDouble).
      map(x => x + 1).
      map(x => x * 2).
      map(x => x + "!")

  println(func(123))
  // res10: String = 248.0!
}
