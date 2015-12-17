package util

import java.util.concurrent.atomic.AtomicInteger

import concurrent.{Await, ExecutionContext, Future, Promise}
import org.scalacheck._

import scala.util.{Success, Failure, Try}

object ft {

  def booleanReduce(futures:Seq[Future[Boolean]], zero:Boolean)(implicit ec: ExecutionContext): Future[Boolean] = {
    if(futures.isEmpty) Future.failed(new RuntimeException("Reduce is called on empty sequence"))
    else {
      val found = Future.find(futures)(_!=zero)
      for {r <- found } yield { r.getOrElse(zero) }
    }
  }
  def reduceWithOr(futures:Seq[Future[Boolean]])(implicit ec: ExecutionContext): Future[Boolean] =
    booleanReduce(futures, zero = false)

  def reduceWithAnd(futures: Seq[Future[Boolean]])(implicit ec: ExecutionContext): Future[Boolean] = {
    booleanReduce(futures, zero = true).flatMap( b =>
      if(!b) {
        Future.successful(false)
      } else {
        Future.fold(futures)(true)(_ && _)
      }
    )
  }
  def futureAnd(futures:Future[Boolean]*)(implicit ec: ExecutionContext):Future[Boolean] = {
  val futuresBuffer = futures.toBuffer
  if (futuresBuffer.isEmpty) Future.successful[Boolean](false)
  else {
    val result = Promise[Boolean]()
    val ref = new AtomicInteger(futuresBuffer.size)
    val search: Try[Boolean] => Unit = v => try {
      v match {
        case  Success(r) if !r  => result tryComplete Success(false)
        case Failure(_) => result tryComplete Success(false)
        case _ =>
      }
    } finally {
      if (ref.decrementAndGet == 0) {
        result tryComplete Success(true)
      }
    }

    futuresBuffer.foreach(_ onComplete search)

    result.future
  }
}
}



