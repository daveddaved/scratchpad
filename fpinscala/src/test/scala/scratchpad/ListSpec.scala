package scratchpad.ch3datastructs.Lists
import org.scalacheck._
//import scratchpad.ch3datastructs.Lists.{Cons, Nil, List}

object ListSpec extends Properties("Lists"){
  import Gen._
  import Prop._
  import Arbitrary._
  import List._

  lazy val genNils = const(Nil)


  val genCons:Gen[Cons[Int]] = for {
    h <- arbitrary[Int]
    t <- frequency((9,genCons),(1,genNils))
    cons <- Cons(h,t)
  } yield cons

  def genList = frequency((15,genCons),(1,genNils))

   property("DBL reversed List equals itself") = forAll(genList) { xs =>
     classify(xs == Nil, "Empty list", "Non Empty list") {
       reverse(reverse(xs)) =? xs
     }
  }

  property("xs append ys has subsequence xs ") = forAll(genList,genList) {(xs,ys) =>
    classify(xs == ys, "List were the same") {
      classify((xs == Nil || ys == Nil) && (xs != ys), "of the time only one of the lists was Nil") {
        hasSubsequence(append(xs, ys), xs)  &&
        hasSubsequence(append(xs, ys), ys)
      }
    }
  }

}
