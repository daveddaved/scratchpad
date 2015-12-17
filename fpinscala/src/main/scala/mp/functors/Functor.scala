package mp.functors

import scala.language.reflectiveCalls


trait Functor[F[_]] { self =>

  def map[A,B](fa: F[A])(f: A => B):F[B]

  def lift[A,B](f:A => B):F[A] => F[B] = fa => map(fa)(f)

  def as[A,B](fa:F[A], b: => B):F[B] =
      map(fa)(_ => b)

  def void[A](fa:F[A]):F[Unit] = as(fa,())

  def compose[G[_]](implicit G:Functor[G]) : Functor[({type f[x] = F[G[x]]})#f] =
    new Functor[({type f[x] = F[G[x]]})#f] {
      def map[A,B](fga: F[G[A]])(f: A => B):F[G[B]] = {
        self.map(fga)(ga => G.map(ga)(a =>f(a)))
      }
    }
}

trait FunctorLaws {

  def identity[F[_], A](fa:F[A])(implicit F:Functor[F]) =
    F.map(fa)(a => a) == fa

  def composition[F[_],A,B,C](fa: F[A], f: A => B, g: B =>C )(implicit F:Functor[F]) =
    F.map(F.map(fa)(f))(g) == F.map(fa)(f andThen g)
}

object Functor {

  implicit val listFunctor: Functor[List] = new Functor[List] {
    override def map[A, B](fa: List[A])(f: (A) => B): List[B] = fa map f
  }

  implicit val optionFunctor: Functor[Option] = new Functor[Option] {
    override def map[A, B](fa: Option[A])(f: (A) => B): Option[B] = fa map f
  }

  implicit def functionFunctor[A]: Functor[({type f[b] = Function[A,b]})#f] =
    new Functor[({type f[b] = Function[A, b]})#f] {
      override def map[AA, B](fa: Function[A, AA])(f: (AA) => B): Function[A, B] = fa andThen f
  }


}