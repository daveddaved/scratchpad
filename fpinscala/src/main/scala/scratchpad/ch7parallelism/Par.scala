package scratchpad.ch7parallelism
import java.util.concurrent._


object Par {
  type Par[A] = ExecutorService => Future[A]

  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

  def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a) // `unit` is represented as a function that returns a `UnitFuture`,
  // which is a simple implementation of `Future` that just wraps a constant value. It doesn't use the `ExecutorService` at all.
  // It's always done and can't be cancelled. Its `get` method simply returns the value that we gave it.
  def lazyUnit[A](a: => A): Par[A] = fork(unit(a)) // TODO probably not needed

  private case class UnitFuture[A](get: A) extends Future[A] {
    def isDone = true
    def get(timeout: Long, units: TimeUnit) = get
    def isCancelled = false
    def cancel(evenIfRunning: Boolean): Boolean = false
  }

  def oldMap2[A,B,C](a: Par[A], b: Par[B])(f: (A,B) => C): Par[C] = // `map2` doesn't evaluate the call to `f` in a separate logical thread,
  // in accord with our design choice of having `fork` be the sole function in the API for controlling parallelism.
  // We can always do `fork(map2(a,b)(f))` if we want the evaluation of `f` to occur in a separate thread.
    (es: ExecutorService) => {
      val af = a(es)
      val bf = b(es)
      UnitFuture(f(af.get, bf.get)) // This implementation of `map2` does _not_ respect timeouts,
      // and eagerly waits for the returned futures. This means that even if you have passed in "forked" arguments,
      // using this map2 on them will make them wait. It simply passes the `ExecutorService` on to both `Par` values,
      // waits for the results of the Futures `af` and `bf`, applies `f` to them, and wraps them in a `UnitFuture`.
      // In order to respect timeouts, we'd need a new `Future` implementation that records the amount of time spent evaluating `af`,
      // then subtracts that time from the available time allocated for evaluating `bf`.
    }

  // 7.4

  def asyncF[A,B](f: A => B ): A => Par[B] = a => lazyUnit(f(a))

  // 7.5

  def sequence[A](ps:List[Par[A]]):Par[List[A]] = ps match {
    case Nil => unit(Nil)
    case h :: t => map2(h, fork(sequence(t)))(_::_)
  }
  def sequence2[A](ps:List[Par[A]]):Par[List[A]] = {
    def go(vector:Vector[Par[A]]):Par[Vector[A]] = {
      val (l, r) = vector.splitAt(vector.length / 2)
      map2(go(l), go(r))(_ ++ _)
    }
    ps match {
      case Nil => unit(Nil)
      case _ => map(go(ps.toVector))(_.toList)
    }
  }

  def parMap[A,B](ps:List[A]) (f:A=>B):Par[List[B]] = fork {
    val fbs = ps.map(asyncF(f))
    sequence(fbs)
  }
// 7.6
  def parFilter[A](as:List[A])(f:A => Boolean):Par[List[A]] =
    parMap(as){ a =>
      if (f(a)) List(a) else List[A]()
    }.map(_.flatten)



  def fork[A](a: => Par[A]): Par[A] = // This is the simplest and most natural implementation of `fork`,
  // but there are some problems with it--for one, the outer `Callable` will block waiting for the "inner" task to complete.
  // Since this blocking occupies a thread in our thread pool, or whatever resource backs the `ExecutorService`,
  // this implies that we're losing out on some potential parallelism. Essentially, we're using two threads when one should suffice.
  // This is a symptom of a more serious problem with the implementation, and we will discuss this later in the chapter.
    es => es.submit(new Callable[A] {
      def call = a(es).get
    })

  def oldMap[A,B](pa: Par[A])(f: A => B): Par[B] =
    map2(pa, unit(()))((a,_) => f(a))

  def flatMap[A,B](pa:Par[A])(f: A => Par[B]) : Par[B] =
    es =>  {
      val ap = pa(es).get
      f(ap)(es)
    }

  def sortPar(parList: Par[List[Int]]) = map(parList)(_.sorted)

  def equal[A](e: ExecutorService)(p: Par[A], p2: Par[A]): Boolean =
    p(e).get == p2(e).get

  def delay[A](fa: => Par[A]): Par[A] =
    es => fa(es)

  def choice[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    es =>
      if (run(es)(cond).get) t(es) // Notice we are blocking on the result of `cond`.
      else f(es)

  def choiceN[A](n: Par[Int])(choices:List[Par[A]]) : Par[A] =
    es => {
      val choice = run(es)(n).get
      run(es)(choices(choice))
    }
  def choiceViaCN[A](cond: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] = {
    val choice = cond.map(if(_) 0 else 1)
      choiceN(choice)(List(t,f))
  }

  def chooser[A,B](pa:Par[A])(choices: A => Par[B]):Par[B] =
    es => {
      val init = run(es)(pa).get
      run(es)(choices(init))
    }
  def map[A,B](pa: Par[A])(f: A => B): Par[B] =
    flatMap(pa)(a1 =>unit(f(a1)))

  def map2[A,B,C](a: Par[A], b: Par[B])(f: (A,B) => C): Par[C] = for {
    a1 <- a
    b1 <- b
  } yield f(a1,b1)

  def map2two[A,B,C](a: Par[A], b: Par[B])(f: (A,B) => C): Par[C] = {
    flatMap(a)(a1 => flatMap(b)(b1 => unit(f(a1,b1))) )
  }

  /* Gives us infix syntax for `Par`. */

  implicit def toParOps[A](p: Par[A]): ParOps[A] = new ParOps(p)

  class ParOps[A](p: Par[A]) {
    def map[B](f: A => B): Par[B] = Par.map(p)(f)
    def flatMap[B](f: A => Par[B]) : Par[B] = Par.flatMap(p)(f)
    def map2[B,C](b: Par[B])(f: (A,B) => C): Par[C] = Par.map2(p,b)(f)
  }
}

object Examples extends  App {
  import Par._
//  def sum(ints: IndexedSeq[Int]): Int = // `IndexedSeq` is a superclass of random-access sequences like `Vector` in the standard library. Unlike lists, these sequences provide an efficient `splitAt` method for dividing them into two parts at a particular index.
//    if (ints.size <= 1)
//      ints.headOption getOrElse 0 // `headOption` is a method defined on all collections in Scala. We saw this function in chapter 3.
//    else {
//      val (l,r) = ints.splitAt(ints.length/2) // Divide the sequence in half using the `splitAt` function.
//      sum(l) + sum(r) // Recursively sum both halves and add the results together.
//    }


  def sum(ints: IndexedSeq[Int]): Par[Int] =
    if (ints.length <= 1)
      Par.unit(ints.headOption getOrElse 0)
    else {
      val (l,r) = ints.splitAt(ints.length/2)
      Par.map2(Par.fork(sum(l)), Par.fork(sum(r)))(_ + _)
    }

  /**
   * val (l,r) = ints.splitAt(ints.length/2)
   * val sumL: Par[Int] = Par.unit(sum(l))
   * val sumR: Par[Int] = Par.unit(sum(r))
   * Par.get(sumL) + Par.get(sumR)
   */


}
