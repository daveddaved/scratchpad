package sorting

import org.scalacheck._
import Prop._
import Sorts._

object QuickSortTests extends Properties("Quick Sort") {


  property("Quick-Sort works on Vectors of Ints") = forAll{(xs:Vector[Int]) =>
    quickSort(xs)(_<_) ?= xs.sorted
  }
  property("Quick-Sort works on Vectors of Doubles") = forAll{(xs:Vector[Double]) =>
    quickSort(xs)(_<_) ?= xs.sorted
  }
  property("Quick-Sort works on Arrays of Doubles") = forAll{(seq:Array[Double]) =>
    quickSort(seq)(_<_) ?= seq.toVector.sorted
  }
  lazy val vecOfalpha = for {
    n <- Gen.choose(0, 1000)
    xs <- Gen.listOfN(n, Gen.alphaStr)
  } yield xs.toVector

  property("Quick-Sort works on Vectors of String") = forAll(vecOfalpha){(seq:Vector[String]) =>
    quickSort(seq)(_.length < _.length).map(_.length) ?= seq.sortBy(_.length).map(_.length)
  }
}
