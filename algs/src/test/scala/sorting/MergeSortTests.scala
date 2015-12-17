package sorting

import org.scalacheck._
import Prop._
import Sorts._

object MergeSortTests extends Properties("Merge Sort") {

  property("Merge-Sorting Ints matches Scala's sort") = forAll {(xs:Vector[Int],ys:Vector[Int]) =>
    (merge(xs.sorted,ys.sorted)(_<_) ?= (ys ++ xs).sorted) &&
      (merge(ys.sorted,xs.sorted)(_<_) ?= (ys ++ xs).sorted)
  }
  property("Merge-Sort works on Vectors of Ints") = forAll{(xs:Vector[Int]) =>
    mergeSort(xs)(_<_) ?= xs.sorted
  }
  property("Merge-Sort works on Vectors of Doubles") = forAll{(xs:Vector[Double]) =>
    mergeSort(xs)(_<_) ?= xs.sorted
  }
  property("Merge-Sort works on Arrays of Doubles") = forAll{(seq:Array[Double]) =>
    mergeSort(seq)(_<_) ?= seq.toVector.sorted
  }
  lazy val vecOfalpha = for {
    n <- Gen.choose(0, 1000)
    xs <- Gen.listOfN(n, Gen.alphaStr)
  } yield xs.toVector
  /**
   * WTF Scala?
   * scala> Vector("q","m").sortWith(_.length <= _.length)
   * res5: scala.collection.immutable.Vector[String] = Vector(m, q)
   */
  lazy val largeVectors = for {
    n <- Gen.choose(10000,100000)
    xs <- Gen.listOfN(n,Gen.posNum[Int])
  } yield xs.toVector

//  property("Sort works on Large Vectors of Ints") = forAll(largeVectors){v =>
//    sort(v)(_<=_) ?= v.sorted
//  }


  property("Merge-Sort works on Vectors of String") = forAll(vecOfalpha){(seq:Vector[String]) =>
    mergeSort(seq)(_.length < _.length).map(_.length) ?= seq.sortBy(_.length).map(_.length)
  }
}


