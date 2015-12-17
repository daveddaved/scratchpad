package mp.monads
import mp.applicative.Applicative

trait Monad[F[_]] extends Applicative[F] {self =>

  def pure[A](a:A):F[A]

  def flatMap[A,B](fa:F[A])(f: A => F[B]):F[B]

  def apply[A,B](fa:F[A])(ff:F[A =>B]):F[B] =
    flatMap(ff)((f:A => B) => map(fa)(f))

  override def map[A,B](fa:F[A])(f:A => B):F[B] =
    flatMap(fa)(a => pure(f(a)))

  def flatten[A](ffa:F[F[A]]):F[A] = flatMap(ffa)(identity)
}
object Monad {

  implicit val listMonad = new Monad[List] {
    def pure[A](a:A) = List(a)
    def flatMap[A,B](fa:List[A])(f: A => List[B]) = fa.flatMap(f)
  }

  implicit val optionMonad = new Monad[Option] {
    def pure[A](a:A) = Some(a)
    def flatMap[A,B](fa:Option[A])(f: A => Option[B]) = fa.flatMap(f)
  }
}