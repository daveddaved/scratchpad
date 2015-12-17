package mp.applicative
import mp.functors.Functor

trait Applicative[F[_]] extends Functor[F]{ self =>

    def pure[A](a:A):F[A]

    def apply[A,B](fa:F[A])(ff:F[A =>B]):F[B]

    def apply2[A,B,Z](fa:F[A],fb:F[B])(f:F[(A,B) =>Z ]):F[Z] =
      apply(fa)(apply(fb)(map(f)(f => b => a => f(a,b))))

    def map[A,B](fa:F[A])(f:A =>B):F[B] =
        apply(fa)(pure(f))

    def map2[A,B,Z](fa:F[A], fb:F[B])(f:(A,B) =>Z):F[Z] =
      apply(fa)(map(fb)(b => f(_,b)))

     def map2a[A,B,Z](fa:F[A], fb:F[B])(f:(A,B) =>Z):F[Z] =
      apply(fa)(
        apply(fb)(
          pure((b:B) => a => f(a,b))
        )
      )

     def map3[A,B,C,Z](fa:F[A],fb:F[B],fc:F[C])(f:(A,B,C) => Z):F[Z] =
        apply(fa)(map2(fb,fc)((b,c) => a => f(a,b,c)))

     def map3a[A,B,C,Z](fa:F[A],fb:F[B],fc:F[C])(f:(A,B,C) => Z):F[Z] =
          apply(fa)(
            apply(fb)(
              apply(fc)(
                pure((c:C) => b => a => f(a,b,c))
              )
            )
          )
     // Helpers
     def tuple2[A,B](fa:F[A],fb:F[B]):F[(A,B)] = map2(fa,fb)((_,_))
     def tuple3[A,B,C](fa:F[A],fb:F[B],fc:F[C]):F[(A,B,C)] = map3(fa,fb,fc)((_,_,_))

     def map4[A,B,C,D,Z](fa:F[A],fb:F[B],fc:F[C],fd:F[D])(f:(A,B,C,D) => Z):F[Z] =
       map2(tuple2(fa,fb),tuple2(fc,fd))({
         case ((a,b),(c,d)) => f(a,b,c,d)
       })

     def flip[A,B](f:F[A => B]): F[A] => F[B] = fa => apply(fa)(f)

      def compose[G[_]](implicit G:Applicative[G]):Applicative[({type f[x] = F[G[x]] })#f] =
        new Applicative[({type f[x] = F[G[x]]})#f] {
          override def pure[A](a: A): F[G[A]] = self.pure(G.pure(a))
          def apply[A, B](fga: F[G[A]])(ff: F[G[A => B]]): F[G[B]] = {
            val x:F[G[A] => G[B]] = self.map(ff)((gab:G[A => B]) => G.flip(gab) )
            self.apply(fga)(x)
          }
        }
}
object Applicative {
  implicit val listApplicative = new Applicative[List] {
    def pure[A](a:A) = List(a)
    def apply[A,B](xs: List[A])(ff:List[A =>B]):List[B] =
        for { x <- xs ; f <- ff } yield f(x)
  }
  implicit val optionApplicative = new Applicative[Option] {
    def pure[A](a:A) = Some(a)
    def apply[A,B](o:Option[A])(ff:Option[A => B]):Option[B] =
      (o,ff) match {
        case (_,None) => None
        case (None,_) => None
        case (Some(a),Some(f)) => pure(f(a))
      }
  }

  implicit val streamApplicative = new Applicative[Stream] {
    def pure[A](a:A) = Stream.continually(a)
    def apply[A,B](str:Stream[A])(fs:Stream[A => B]):Stream[B] =
      (str zip fs).map( { case (a,f) => f(a) })
  }
}

