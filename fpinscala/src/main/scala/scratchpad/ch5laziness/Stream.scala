package scratchpad.ch5laziness
import Stream._

case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]
sealed trait Stream[+A] {
  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, _) => Some(h()) //Option(h())
  }

  /**
   * Hard: Implement headOption using foldRight .
   */
  def headOptionViaFR: Option[A] =
  //   foldRight(None.asInstanceOf[Option[A]]) { (h,_) => Some(h)   }
    foldRight(None: Option[A]) { (h, _) => Some(h) }

  /**
   * Write a function to convert a Stream to a List , which will force its evaluation and let
   * you look at it in the REPL . You can convert to the regular List type in the standard
   * library. You can place this and other functions that operate on a Stream inside the
   * Stream trait.
   */
  def toListR: List[A] = this match {
    case Empty => Nil
    case Cons(h, t) => h() :: t().toListR
  }

  def toList: List[A] = {
    def go(acc: List[A], stream: Stream[A]): List[A] = {
      stream match {
        case Empty => acc
        case Cons(h, t) => go(acc :+ h(), t())
      }
    }
    go(Nil, this)
  }

  /**
  Write the function take(n) for returning the first n elements of a Stream , and
  drop(n) for skipping the first n elements of a Stream .
    */
  def takeToList(n: Int): List[A] = {
    def go(acc: List[A], count: Int, st: Stream[A]): List[A] =
      st match {
        case Cons(h, t) if count > 1 => go(acc :+ h(), count - 1, t())
        case Cons(h, t) if count == 1 => go(acc :+ h(), count - 1, empty)
        case _ => acc
      }
    go(Nil, n, this)
  }

  def take(n: Int): Stream[A] =
    this match {
      case Cons(h, t) if n == 1 => cons(h(), empty[A])
      case Cons(h, t) if n > 1 => cons(h(), t().take(n - 1))
      case _ => empty[A]
    }

  def toListFast: List[A] = {
    val buf = new collection.mutable.ListBuffer[A]
    @annotation.tailrec
    def go(s: Stream[A]): List[A] = s match {
      case Cons(h, t) =>
        buf += h()
        go(t())
      case _ => buf.toList
    }
    go(this)
  }

  def drop(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 0 => t().drop(n - 1)
    case _ => this
  }

  /**
   * Write the function takeWhile for returning all starting elements of a Stream that
   * match the given predicate.
   */
  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
    case _ => empty
  }

  /**
   * Use foldRight to implement takeWhile .
   */
  def takeWhileViaFR(p: A => Boolean): Stream[A] =
    foldRight(empty[A]) { (h, t) => if (p(h)) cons(h, t) else empty[A] }


  def exists2(p: A => Boolean): Boolean = this match {
    case Cons(h, t) => p(h()) || t().exists(p)
    case _ => false
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B =
    this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f)) // t().foldRight(f(h(), z))(f)
      case _ => z
    }

  def exists(p: A => Boolean): Boolean =
    foldRight(false) { (a, b) => p(a) || b }

  /**
   * Implement forAll , which checks that all elements in the Stream match a given predi-
   * cate. Your implementation should terminate the traversal as soon as it encounters a
   * nonmatching value
   */
  def forAll(p: A => Boolean): Boolean = foldRight(false)((r, l) => p(r) && l)

  /**
   * Implement map , filter , append , and flatMap using foldRight . The append method
   * should be non-strict in its argument.
   */
  def map[B](f: A => B): Stream[B] =
    foldRight(empty[B]) { (h, t) => cons(f(h), t) }

  def filter(p: A => Boolean): Stream[A] =
    foldRight(empty[A]) { (h, t) => if (p(h)) cons(h, t) else t }


  def append[B >: A](s: => Stream[B]): Stream[B] =
    foldRight(s) { (h, t) => cons(h, t) }

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B]) { (h, t) => f(h) append (t) }

  def find(p: A => Boolean): Option[A] = filter(p).headOption

  /**
   * Use unfold to implement map, take, takeWhile, zipWith (as in chapter 3), and zipAll.
   * The zipAll function should continue the traversal as long as either stream has more elements—it
   * uses Option to indicate whether each stream has been exhausted.
   */
  def mapuf[B](f: A => B): Stream[B] =
    unfold(this) { case Cons(h, t) => Some(f(h()), t()); case _ => None }

  def takeuf(n: Int): Stream[A] =
    unfold(n, this) {
      case (i, Cons(h, t)) if i == 1 => Some(h(), (0, empty))
      case (i, Cons(h, t)) if i > 1 => Some(h(), (i - 1, t()))
      case _ => None
    }

  def takeWhileUF(p: A => Boolean): Stream[A] =
    unfold(this) {
      case (Cons(h, t)) if p(h()) => Some(h(), t())
      case _ => None
    }

  def zipWith[B, C](that: Stream[B])(f: (A, B) => C): Stream[C] =
    unfold(this, that) {
      //      case (Empty,Empty) => None
      //      case (_, Empty) => None
      //      case (Empty,_) => None
      case (Cons(h, t), Cons(hh, tt)) => Some((f(h(), hh()), (t(), tt())))
      case _ => None
    }

  def zipWithAll[B](that: Stream[B]): Stream[(Option[A], Option[B])] =
    unfold(this, that) {
      case (Empty, Empty) => None
      case (Cons(h, t), Empty) => Some((Some(h()), None), (t(), empty[B]))
      case (Empty, Cons(h, t)) => Some((None, Some(h())), (empty[A], t()))
      case (Cons(h, t), Cons(hh, tt)) => Some((Some(h()), Some(hh())), (t(), tt()))
    }

  def startsWith[A](s: Stream[A]): Boolean =
    (this, s) match {
      case (Empty, Empty) => true
      case (_, Empty) => false
      case _ => !(this zipWithAll s).exists({ case (o1, o2) => o1.isEmpty || (o1 != o2 && o2.isDefined) })
    }

  //
  //    (this,s) match {
  //      case (Empty,Empty) => true
  //      case (_,Empty) => false
  //      case (Empty,_) => false
  //      case _=> ! this.zipWith(s)(_==_).exists(_== false)
  //    }
  //  zipAll(s).takeWhile(!_._2.isEmpty) forAll {
  //    case (h,h2) => h == h2
  //  }
  /** Implement tails using unfold. For a given Stream, tails returns
    * the Stream of suf- fixes of the input sequence, starting with the original Stream.
    * For example, given Stream(1,2,3), it would return Stream(Stream(1,2,3), Stream(2,3), Stream(3), Stream()).
    */

  def tails: Stream[Stream[A]] = {
    def go(s: Stream[A]): Stream[Stream[A]] =
      unfold(s) {
        case Empty => None
        case st => Some((st, st drop 1))
      }
    go(this).append(Stream(empty[A]))
  }

  def hasSubsequence[A](s: Stream[A]): Boolean = tails exists (_ startsWith s)

  /**
   * Hard: Generalize tails to the function scanRight,
   * which is like a foldRight that returns a stream of the intermediate results. For example:
   * scala> Stream(1,2,3).scanRight(0)(_ + _).toList
   * res0: List[Int] = List(6,5,3,0)
   */
  def scanRightBoom[B](z: => B)(f: (A, => B) => B): Stream[B] =
    tails.map(st => st.foldRight(z)(f))

  def scanRight[B](z: => B)(f: (A, => B) => B): Stream[B] = this match {
    case Empty => empty[B]
    case _ =>
      foldRight(Stream(z)) {
        case (a, acc) => acc match {
          case Empty => empty[B]
          case Cons(h,t) => cons(f(a, h()), acc) //Stream(f(a, h())) append acc
        }
      }
  }
//  Theirs 
  def scanRightRight[B](z: B)(f: (A, => B) => B): Stream[B] =
    foldRight((z, Stream(z)))((a, p0) => {
      // p0 is passed by-name and used in by-name args in f and cons. So use lazy val to ensure only one evaluation...
      lazy val p1 = p0
      val b2 = f(a, p1._1)
      (b2, cons(b2, p1._2))
    })._2

}
object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail )
  }

  def empty[A]: Stream[A] = Empty
  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  val ones:Stream[Int] = Stream.cons(1,ones)
  /**
   * Generalize ones slightly to the function constant , which returns an infinite Stream of
   * a given value.
   */
  def constant[A](a: A): Stream[A] = cons(a, constant(a))

  /**
   * Write a function that generates an infinite stream of integers, starting from n ,
   * then n + 1 , n + 2 , and so on. 7
   */
  def from(n: Int): Stream[Int] = cons(n, from(n + 1))

  /**
   * Write a function fibs that generates the infinite stream of Fibonacci numbers: 0, 1, 1,
   * 2, 3, 5, 8, and so on.
   */
  def fibs:Stream[BigInt] = {
    def fib(cur: BigInt, next: BigInt):Stream[BigInt] = cons(cur, fib(next, next + cur))
    fib(0, 1)
  }

  /**
   * Write a more general stream-building function called unfold .
   * It takes an initial state, and a function for producing both the next state
   * and the next value in the generated stream.
   */
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z).fold(empty[A]) { case (a,s) => cons(a, unfold(s)(f)) }

  /**
   * Write fibs , from , constant , and ones in terms of unfold . 8
   */
   def from2(n:Int) = unfold(n){ n => Some((n,n+1)) }
   def constant2[T](t:T) = unfold(t){ _=> Some(t,t) }
   lazy val ones2 = unfold(1){ _=> Some(1,1) }
   lazy val fibs2 = unfold(0,1) { case (p,c) => Some(p, (c, p + c)) }
}

object testStreams extends App {
  import Stream._

//  fibs2.take(9).zipWith(fibs.take(10))((_,_)).takeWhileUF(_._1 < 100).toList foreach println
//  (from(1).take(13) zipWithAll from(1).take(10) ).toList foreach println
//  //println(Stream(1,2,3,4) zipWithAll Stream(1,2) exists({case (o1,o2) =>  o1 != o2 }))
//  println(Stream(1,2,3,4) startsWith Stream(1,2))
//  //(Stream(1,2,3,4).tails).toList foreach (s => println(s.toList) )
//  println("SubSequence    : " + from(1000).take(1000).hasSubsequence(from(1900).take(10)))
//  println("ScanRightBoom  : " + from(10).take(5).scanRight(0)(_ + _).toList)
//  println("ScanRight      : " + from(10).take(5).scanRight(0)(_ + _).toList)
//  println("ScanRightRight : " + from(10).take(5).scanRightRight(0)(_ + _).toList)
  // SO SOS with any implementation fibs2.scanRight(0)(_ + _).take(10).toList
  println(from(1).take(10).tails.take(5).map(_.toListFast).toListFast)
}
