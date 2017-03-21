package mp

import mp.semigroup.Semigroup
import mp.monoid.Monoid

object Test {

  def combine[A:Semigroup](a:A,b:A):A =
    Semigroup[A].add(a,b)

  def aggregate[A:Monoid](xs: Iterable[A]):A = {
    xs.fold(Monoid[A].zero)(Monoid[A].add)
  }

  def main(args: Array[String]): Unit = {
    val ints = combine(1,2)
    val strings = combine("aaa", "bbb")
    val mstrings = aggregate(List("asd", "dasd","dad"))
    println(ints)
    println(strings)
    println(mstrings)
  }
}
