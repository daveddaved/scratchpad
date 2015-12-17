package scratchpad.ch12applicative



import scratchpad.ch11monads.Functor
import scratchpad.ch6state.State
import scratchpad.ch10monoids._
import language.reflectiveCalls


trait Applicative[F[_]] extends Functor[F] {

  def map2[A,B,C](fa:F[A], fb:F[B])(f: (A,B) => C ):F[C] =
      apply(
        apply(
          unit(f.curried)
        )(fa)
      )(fb)


  def unit[A](a: => A):F[A]

  def map[A,B](fa:F[A])(f: A => B) = map2(fa, unit(()))((a, _) => f(a))

  def traverse[A,B](as: List[A])(f: A => F[B]): F[List[B]] =
      as.foldRight(unit(List[B]()))((a, fbs) => map2(f(a), fbs)(_ :: _))

  def sequence[A](fas: List[F[A]]): F[List[A]] = traverse(fas)(fa => fa)

  def sequenceMap[K,V](ofa: Map[K,F[V]]): F[Map[K,V]] = {
    ofa.foldLeft(unit(Map[K,V]()))({
      case (l,(k,v)) =>
        map2(l,v)((m,v1) => m + (k -> v1) )
    }
    )
  }

  def replicateM[A](n: Int, fa: F[A]): F[List[A]] =
      if (n <= 0)
        unit(List[A]())
      else
        map2(fa, replicateM(n -1, fa))(_ :: _)
  /*
   * Hard: The name applicative comes from the fact that we can formulate the Applicative
   * interface using an alternate set of primitives, unit and the function apply, rather than
   * unit and map2. Show that this formulation is equivalent in expressiveness by defining
   * map2 and map in terms of unit and apply. Also establish that apply can be implemented
   * in terms of map2 and unit.
   */
  def apply[A,B](fab: F[A => B])(fa: F[A]): F[B] = map2(fab,fa)((f2b,a) => f2b(a))
  def apply[A,B,Z](ab2z: F[(A,B) => Z])(fa:F[A], fb:F[B]):F[Z] =
        apply(
          apply(
            map(ab2z)(f => (b:B) => (a:A) => f(a,b))
          )(fb)
        )(fa)

  def product[A,B](fa: F[A], fb: F[B]): F[(A,B)] = map2(fa,fb)((_,_))
  def product[G[_]](G: Applicative[G]): Applicative[({type f[x] = (F[x], G[x])})#f] = {
    val self = this
    new Applicative[({type f[x] = (F[x], G[x])})#f] {
      def unit[A](a: => A) = (self.unit(a), G.unit(a))
      override def apply[A,B](fs: (F[A => B], G[A => B]))(p: (F[A], G[A])) =
        (self.apply(fs._1)(p._1), G.apply(fs._2)(p._2))
    }
  }
  /*
  The apply method is useful for implementing map3, map4, and so on, and the pattern
  is straightforward. Implement map3 and map4 using only unit, apply, and the curried
  method available on functions
  */
  def map3[A,B,C,D](fa: F[A],
                    fb: F[B],
                    fc: F[C])(f: (A, B, C) => D): F[D] =
      apply(
        apply(
          apply(
            unit(f.curried)
          )(fa)
        )(fb)
      )(fc)

  def map4[A,B,C,D,E](fa: F[A],
                      fb: F[B],
                      fc: F[C],
                      fd: F[D])(f: (A, B, C, D) => E): F[E] =
      apply(
        apply(
          apply(
            apply(
              unit(f.curried)
            )(fa)
          )(fb)
        )(fc)
      )(fd)

}


trait Monad[F[_]] extends Applicative[F] {
  def flatMap[A,B](fa: F[A])(f: A => F[B]): F[B] = join(map(fa)(f))
  def join[A](ffa: F[F[A]]): F[A] = flatMap(ffa)(fa => fa)
  def compose[A,B,C](f: A => F[B], g: B => F[C]): A => F[C] =
    a => flatMap(f(a))(g)

  override def map[A,B](fa: F[A])(f: A => B): F[B] =
    flatMap(fa)((a: A) => unit(f(a)))

  override def map2[A,B,C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
    flatMap(fa)(a => map(fb)(b => f(a,b)))

  def **[A,B,C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = map2(fa,fb)(f)
}

object Applicative {

  //Write a monad instance for Either.
  def eitherMonad[E]: Monad[({type f[x] = Either[E, x]})#f] = {
    new Monad[({type f[x] = Either[E, x]})#f] {
      override def unit[A](a: => A): Either[E, A] = Right(a)

      override def flatMap[A, B](either: Either[E, A])(f: A => Either[E, B]) =
        either match {
          case Right(right) => f(right)
          case Left(left) => Left(left)
        }
    }
  }

  sealed trait Validation[+E, +A]

  case class Failure[E](head: E, tail: Vector[E] = Vector())
    extends Validation[E, Nothing]

  case class Success[A](a: A) extends Validation[Nothing, A]

  def validationApplicative[E]: Applicative[({type f[x] = Validation[E, x]})#f] =
    new Applicative[({type f[x] = Validation[E, x]})#f] {
      def unit[A](a: => A) = Success(a)

      override def map2[A, B, C](fa: Validation[E, A], fb: Validation[E, B])(f: (A, B) => C) =
        (fa, fb) match {
          case (Success(a), Success(b)) => Success(f(a, b))
          case (Failure(h1, t1), Failure(h2, t2)) =>
            Failure(h1, t1 ++ Vector(h2) ++ t2)
          case (e@Failure(_, _), _) => e
          case (_, e@Failure(_, _)) => e
        }
    }

  val optionApplicative: Applicative[Option] =
    new Applicative[Option] {
      def unit[A](a: => A) = Some(a)
      override def map2[A, B, C](fa: Option[A], fb: Option[B])(f: (A,B) => C) =
        (fa,fb) match {
          case (None,Some(_)) => None
          case (Some(_),None) => None
          case (None,None) => None
          case (Some(a),Some(b)) => Option(f(a,b))
        }
    }
}

object Monad {

  // Notice that in the case of a `Left`, flatMap does nothing.
  def eitherMonad[E]: Monad[({type f[x] = Either[E, x]})#f] =
    new Monad[({type f[x] = Either[E, x]})#f] {
      def unit[A](a: => A): Either[E, A] = Right(a)
      override def flatMap[A,B](eea: Either[E, A])(f: A => Either[E, B]) = eea match {
        case Right(a) => f(a)
        case Left(b) => Left(b)
      }
    }

  def stateMonad[S] = new Monad[({type f[x] = State[S, x]})#f] {
    def unit[A](a: => A): State[S, A] = State(s => (a, s))
    override def flatMap[A,B](st: State[S, A])(f: A => State[S, B]): State[S, B] =
      st flatMap f
  }

  // Monad composition
  def composeM[G[_],H[_]](implicit G: Monad[G], H: Monad[H], T: Traverse[H]):
  Monad[({type f[x] = G[H[x]]})#f] = new Monad[({type f[x] = G[H[x]]})#f] {
    def unit[A](a: => A): G[H[A]] = G.unit(H.unit(a))
    override def flatMap[A,B](mna: G[H[A]])(f: A => G[H[B]]): G[H[B]] =
      G.flatMap(mna)(na => G.map(T.traverse(na)(f))(H.join))
  }

  case class OptionT[M[_],A](value: M[Option[A]])(implicit M: Monad[M]) {
    def flatMap[B](f: A => OptionT[M, B]): OptionT[M, B] =
      OptionT(M.flatMap(value) {
        case None => M.unit(None)
        case Some(a) => f(a).value
      })
  }

}

trait Traverse[F[_]] extends Functor[F] with Foldable[F] { self =>
  def traverse[G[_]:Applicative,A,B](fa: F[A])(f: A => G[B]): G[F[B]] =
    sequence(map(fa)(f))
  def sequence[G[_]:Applicative,A](fga: F[G[A]]): G[F[A]] =
    traverse(fga)(ga => ga)
  type Id[A] = A

  val idMonad = new Monad[Id] {
    def unit[A](a: => A) = a
    override def flatMap[A,B](a: A)(f: A => B): B = f(a)
  }

  def map[A,B](fa: F[A])(f: A => B): F[B] =
    traverse[Id, A, B](fa)(f)(idMonad)

  type Const[M, B] = M

  implicit def monoidApplicative[M](M: Monoid[M]):Applicative[({ type f[x] = Const[M, x] })#f] =
    new Applicative[({ type f[x] = Const[M, x] })#f] {
      def unit[A](a: => A): M = M.zero
      override def map2[A,B,C](m1: M, m2: M)(f: (A,B) => C): M = M.op(m1,m2)
    }

  override def foldMap[A,M](as: F[A])(f: A => M)(mb: Monoid[M]): M =
    traverse[({type f[x] = Const[M,x]})#f,A,Nothing](as)(f)(monoidApplicative(mb))

  def traverseS[S,A,B](fa: F[A])(f: A => State[S, B]): State[S, F[B]] =
    traverse[({type f[x] = State[S,x]})#f,A,B](fa)(f)(Monad.stateMonad)

  import State._
  def _zipWithIndex[A](ta: F[A]): F[(A,Int)] =
    traverseS(ta)((a: A) => for {
      i <- get[Int]
      _ <- set(i + 1)
    } yield (a, i)).run(0)._1

  def stateToList[A](fa: F[A]): List[A] =
    traverseS(fa)((a: A) => for {
      as <- get[List[A]]
      _ <- set(a :: as)
    } yield ()).run(Nil)._2.reverse

  def mapAccum[S,A,B](fa: F[A], s: S)(f: (A, S) => (B, S)): (F[B], S) =
    traverseS(fa)((a: A) => for {
      s1 <- get[S]
      (b, s2) = f(a, s1)
      _ <- set(s2)
    } yield b).run(s)

  override def toList[A](fa: F[A]): List[A] =
    mapAccum(fa, List[A]())((a, s) => ((), a :: s))._2.reverse

  def zipWithIndex[A](fa: F[A]): F[(A, Int)] =
    mapAccum(fa, 0)((a, s) => ((a, s), s + 1))._1

  def reverse[A](fa: F[A]): F[A] =
    mapAccum(fa, toList(fa).reverse)((_, as) => (as.head, as.tail))._1

  override def foldLeft[A,B](fa: F[A])(z: B)(f: (B, A) => B): B =
    mapAccum(fa, z)((a, b) => ((), f(b, a)))._2

  def zip[A,B](fa: F[A], fb: F[B]): F[(A, B)] =
    mapAccum(fa, toList(fb)) {
      case (a, Nil) => sys.error("zip: Incompatible shapes.")
      case (a, b :: bs) => ((a, b), bs)
    }._1

  def zipL[A,B](fa: F[A], fb: F[B]): F[(A, Option[B])] =
    mapAccum(fa, toList(fb)) {
      case (a, Nil) => ((a, None), Nil)
      case (a, b :: bs) => ((a, Some(b)), bs)
    }._1

  def zipR[A,B](fa: F[A], fb: F[B]): F[(Option[A], B)] =
    mapAccum(fb, toList(fa)) {
      case (b, Nil) => ((None, b), Nil)
      case (b, a :: as) => ((Some(a), b), as)
    }._1

//  Use applicative functor products to write the fusion of two traversals. This function
//    will, given two functions f and g, traverse fa a single time, collecting the results of
//    both functions at once.

  def fuse[M[_],N[_],A,B](fa: F[A])(f: A => M[B], g: A => N[B])
                         (implicit M: Applicative[M], N: Applicative[N]): (M[F[B]], N[F[B]]) =
    traverse[({type f[x] = (M[x], N[x])})#f, A, B](fa)(a => (f(a), g(a)))(M product N)
//  Implement the composition of two Traverse instances.
  def compose[G[_]](implicit G: Traverse[G]): Traverse[({type f[x] = F[G[x]]})#f] =
  new Traverse[({type f[x] = F[G[x]]})#f] {
    override def traverse[M[_]:Applicative,A,B](fa: F[G[A]])(f: A => M[B]) =
      self.traverse(fa)((ga: G[A]) => G.traverse(ga)(f))
  }
}


object testApplicative extends App {
  import java.text._
  import java.util.Date
  import scala.util.control.NonFatal
  import Applicative._


  val traverseList = new Traverse[List] {
    override def traverse[M[_],A,B](as: List[A])(f: A => M[B])(implicit M: Applicative[M]): M[List[B]] =
      as.foldRight(M.unit(List[B]()))((a, fbs) => M.map2(f(a), fbs)(_ :: _))
  }

  val traverseOption = new Traverse[Option] {
    override def traverse[M[_],A,B](o:Option[A])(f: A => M[B])(implicit M: Applicative[M]): M[Option[B]] =
      o.fold(M.unit(None.asInstanceOf[Option[B]]))(a => M.map(f(a))(Some(_)))
  }
  println(traverseOption.traverse(Some(1))(a => Option(a + 1))(optionApplicative))

  case class Tree[+A](head: A, tail: List[Tree[A]])

  val treeTraverse = new Traverse[Tree] {
    override def traverse[M[_],A,B](ta: Tree[A])(f: A => M[B])(implicit M: Applicative[M]): M[Tree[B]] =
      M.map2(f(ta.head), traverseList.traverse(ta.tail)(a => traverse(a)(f)))(Tree(_, _))
  }


  val streamApplicative = new Applicative[Stream] {
    def unit[A](a: => A): Stream[A] =
      Stream.continually(a)

    override def map2[A,B,C](a: Stream[A], b: Stream[B])(
      f: (A,B) => C): Stream[C] =
      a zip b map f.tupled
  }
  //  Hard: What is the meaning of streamApplicative.sequence? Specializing the signature
  //  of sequence to Stream, we have this:
  //  def sequence[A](a: List[Stream[A]]): Stream[List[A]]
  println(streamApplicative.sequence(List(Stream.continually(1),Stream.continually(2),Stream.continually(3))).take(5))

  case class WebForm(name: String, birthdate: Date, phoneNumber: String)

  def validName(name: String): Validation[String, String] =
    if (name != "") Success(name)
    else Failure("Name cannot be empty")

  def validBirthdate(birthdate: String): Validation[String, Date] =
    try {

      Success((new SimpleDateFormat("yyyy-MM-dd")).parse(birthdate))
    } catch {
      case NonFatal(e) => Failure(s"Birthdate must be in the form yyyy-MM-dd: $e")
    }
  def validPhone(phoneNumber: String): Validation[String, String] =
    if (phoneNumber.matches("[0-9]{10}"))
      Success(phoneNumber)
    else Failure("Phone number must be 10 digits")

  def validWebForm(name: String,
                   birthdate: String,
                   phone: String): Validation[String, WebForm] = {
    val V = validationApplicative[String]
    V.map3(
      validName(name),
      validBirthdate(birthdate),
      validPhone(phone))(
      WebForm(_, _, _))
  }

  val forms = List(("","aa","1-1-1"),("AA","000111","1-1-1"),("AA","2010-01-01","0123456789"))
  forms.map(validWebForm _ tupled) foreach println

  val V = validationApplicative[String]
  val WF = validationApplicative[WebForm]


  //println("Seq : " + V.sequence(forms))
}