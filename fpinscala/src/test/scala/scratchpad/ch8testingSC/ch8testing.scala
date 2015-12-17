package ch8testingSC

import org.scalacheck.Prop._
import org.scalacheck._

object ch8testing extends Properties("Chapter8") {

  def sum(xs:List[Int]):Int = xs.map(identity).sum


  val lists = for {
    i <- Gen.posNum[Int]
    list <- Gen.listOfN(i, Gen.choose(Int.MinValue,Int.MaxValue))
  } yield list

  val sameValList:Gen[List[Int]] = for {
    i <- Gen.posNum[Int]
    v <- Arbitrary.arbitrary[Int]
    list <- Gen.listOfN(i, Gen.const(v))
  } yield list

  val posList:Gen[List[Int]] = for {
    i <- Gen.posNum[Int]
    v <- Gen.posNum[Int]
    list <- Gen.listOfN(i, Gen.const(v))
  } yield list

  val sumRev = forAll(lists) { xs =>
    sum(xs) == sum(xs.reverse)
  }
  val sumGtOrEqZero = forAll(posList) { xs =>
    sum(xs) >= 0
  }
  val ssumRev = forAll(sameValList) { xs =>
    sum(xs) == xs.head * xs.length
  }
  val maxIsMax = forAll(lists){xs =>
    val max = xs.max
    ! xs.exists(_ > max)
  }
  val maxIsMax2 = forAll(lists){xs =>
    val max = xs.max
    xs.forall(_ <= max)
  }

  property("Sum") = sumGtOrEqZero && sumRev && ssumRev
  property("Max") = maxIsMax && maxIsMax2

}
