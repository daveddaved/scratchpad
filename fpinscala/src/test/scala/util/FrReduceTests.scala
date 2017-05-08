package util

import org.scalacheck.Prop._
import org.scalacheck.Prop.throws
import org.scalacheck.{ Gen, Properties }
import Gen._

import scala.concurrent.{ Await, Future }
import ft._
import concurrent.ExecutionContext //Implicits.global
import scala.concurrent.duration._
//import java.util.concurrent.Executors
//import java.util.concurrent.atomic.
trait BooleanReduceSpec {
  val cores = Runtime.getRuntime().availableProcessors()
  val err = 5
  implicit val ec = ExecutionContext.fromExecutor(java.util.concurrent.ForkJoinPool.commonPool()) //Executors.newFixedThreadPool(cores))
  @volatile var results = List[Int]()
  def waitForAllToFinish[A](futures: Seq[Future[A]], time: Int): Unit = Await.ready(Future.sequence(futures), time seconds)

  val genPair: Gen[(Int, Boolean)] = for {
    i <- choose(1, 10)
    b <- oneOf(true, false)
  } yield (i, b)

  val listGen = for {
    size <- choose(0, 1000) // completion order is unpredictable with set larger than number of cores
    xs <- listOfN(size, genPair)
  } yield xs

  def completedOp(xs: List[(Int, Boolean)]): Int
  def completedAll(xs: List[(Int, Boolean)]): Int
  def expected(xs: List[(Int, Boolean)]): Boolean
}

object futureAndTests extends Properties("Reduce with And") with BooleanReduceSpec {
  def completedOp(xs: List[(Int, Boolean)]): Int = xs.filterNot(_._2).map(_._1).min
  def completedAll(xs: List[(Int, Boolean)]): Int = xs.map(_._1).sum
  def expected(xs: List[(Int, Boolean)]): Boolean = xs.nonEmpty && xs.forall(_._2)
  def futures(xs: List[(Int, Boolean)]): List[Future[Boolean]] = xs.map({ pair =>
    val (timeout, value) = pair
    Future {
      Thread.sleep(timeout)
      if (value) results = results :+ timeout
      value
    }
  })
  property("Returns True only if all futures return true") = forAll(listGen) { xs =>
    classify(xs.isEmpty, "empty list", "non empty list") {
      Await.result(futureAnd(futures(xs): _*), xs.size * 10 seconds) == expected(xs)
    }
  }
  property("Completes faster than flatMap ") = forAll(listGen) { xs =>
    true
  }
}

