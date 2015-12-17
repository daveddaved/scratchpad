package sorting

import scala.annotation.tailrec
import scala.util.control.TailCalls._

object Sorts {

  final def merge[T](seq1: IndexedSeq[T], seq2: IndexedSeq[T])(implicit cp: (T, T) => Boolean): IndexedSeq[T] = {
    @tailrec
    def loop(s1: IndexedSeq[T], s2: IndexedSeq[T], acc: IndexedSeq[T]): IndexedSeq[T] = {
      (s1, s2) match {
        case (IndexedSeq(), l) => acc ++ l
        case (l, IndexedSeq()) => acc ++ l
        case ((x +: xs), (y +: ys)) =>
          if (cp(x, y)) loop(xs, s2, acc :+ x)
          else loop(s1, ys, acc :+ y)
      }
    }
    loop(seq1, seq2, Vector.empty[T])
  }

  final def mergeSort[T](xs: IndexedSeq[T])(implicit cp: (T, T) => Boolean): IndexedSeq[T] = {

    // make this not oom on large seqs
    def go(xs: IndexedSeq[T])(implicit cp: (T, T) => Boolean): TailRec[IndexedSeq[T]] = {
      val middle = xs.length / 2
      if (middle == 0) done(xs)
      else {
        val (l, r) = xs splitAt middle
        for {
          lp <- tailcall(go(l))
          rp <- tailcall(go(r))
        } yield merge(lp, rp)
      }
    }
    go(xs)(cp).result
  }

  final def sortViaStreams[T](xs: Stream[T])(cp: (T, T) => Boolean): Stream[T] = {
    val m = xs.length / 2
    if (m == 0) xs
    else {
      def merge(ls: Stream[T], rs: Stream[T]): Stream[T] = (ls, rs) match {
        case (Stream.Empty, _) => rs
        case (_, Stream.Empty) => ls
        case (l #:: ls1, r #:: rs1) =>
          if (cp(l, r)) l #:: merge(ls1, rs)
          else r #:: merge(ls, rs1)
      }
      val (l, r) = xs splitAt m
      merge(sortViaStreams(l)(cp), sortViaStreams(r)(cp))
    }
  }

  def quickSort[T](xs: Seq[T])(cp:(T,T) => Boolean): Seq[T] = {
    def go(ys: Seq[T]): TailRec[Seq[T]] =
      ys match {
        case Nil => done(List[T]())
        case head +: tail => {
          val (low, high) = tail.partition(cp(_, head))
          for {
            l <- tailcall[Seq[T]](go(low))
            h <- tailcall[Seq[T]](go(high))
          } yield l ++ (head +: h)
        }
      }
      go(xs).result
}

  def qs2(xs: IndexedSeq[Int]): IndexedSeq[Int] = {
    val sz = xs.length
    if (sz < 2) xs
    else {
      val pivot = xs(sz/2)
          qs2(xs.filter(_< pivot)) ++
          xs.filter(_== pivot) ++
          qs2(xs.filter(_ > pivot))
    }
  }
 def qs3(xs: Array[Float]): Array[Float] = {
   def swap(i: Int, j: Int) {
     val t = xs(i); xs(i) = xs(j); xs(j) = t
   }

   def sort1(l: Int, r: Int) {
     val pivot = xs((l + r) / 2)
     var i = l; var j = r
     while (i <= j) {
       while (xs(i) < pivot) i += 1
       while (xs(j) > pivot) j -= 1
       if (i <= j) {
         swap(i, j)
         i += 1
         j -= 1
       }
     }
     if (l < j) sort1(l, j)
     if (j < r) sort1(i, r)
   }
   sort1(0, xs.length - 1)
   xs
 }

}
object testMs extends App {

  val us = Array.fill(10000000)(scala.util.Random.nextFloat())
  //println(Sorts.quickSort(us)(_<_))
 // assert(Sorts.qs2(us) == us.sortWith(_<_))
  println(Sorts.qs3(us).length)
}
