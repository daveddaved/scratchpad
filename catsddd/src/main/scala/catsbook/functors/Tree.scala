package catsbook.functors
import cats.Functor
import cats.syntax.functor._

sealed trait Tree[+A]
final case class Branch[A](left: Tree[A], right: Tree[A])
  extends Tree[A]
final case class Leaf[A](value: A) extends Tree[A]

object Tree {

  def branch[A](left: Tree[A], right: Tree[A]): Tree[A] =
    Branch(left, right)

  def leaf[A](value: A): Tree[A] =
    Leaf(value)

  implicit val treeFunctor = new Functor[Tree] {
    def map[A,B](tree:Tree[A])(f:A ⇒ B):Tree[B] =
      tree match {
        case Branch(l,r) ⇒ Branch(map(l)(f), map(r)(f))
        case Leaf(v) ⇒ Leaf(f(v))
      }
  }

}

object tstTree extends App  {
  import Tree._

  val tree = branch(
    branch(leaf(1), leaf(2)),
    branch(
      branch(
        leaf(11),
        branch(
          leaf(22),
          leaf(44)
        )
      ),
      leaf(99)
    )
  )
  import catsbook.Printable._
  tree.map(_ + 100).map(_.print)
}