package scratchpad
object shapeLight {

  sealed trait HList

  sealed trait HNil extends HList {
    def ::[H](h:H):H :: HNil = new ::(h,this)
  }

  case object HNil extends HNil

  case class ::[+H, +T <: HList](head: H, tail: T) extends HList {
    def ::[G](g: G): G :: H :: T = new ::(g, this)
  }

}
object Opt2 extends App {
  def zip2[A](o1:Option[A], o2:Option[A])(f:(A,A) => A):Option[A] =
    o1.fold(o2)(a =>
       o2.fold(o1)(b =>
                  Option(f(a,b)))
    )
  def zipadd[A:Numeric](o1:Option[A],o2:Option[A]):Option[A]=zip2(o1,o2)(implicitly[Numeric[A]].plus(_,_))

  println(zip2(Some(1),Some(2))(_+_))
  println(zip2(None,Some(2))(_+_))
  println(zip2(Some(1),None)(_+_))
  println(zip2[Int](None,None)(_+_))
  println(List[Option[Int]](None,Some(1),Some(2),Some(2),None,Some(10)).reduce(zipadd[Int]))
  val xs:List[Int] = List(1,2,3,4,5)
  val ys = xs.map(x => if((x % 2) ==0) Some(x) else None).reduce(zipadd[Int])
  println(ys)
  val ls = List(1,2,3,4,5,6,7,8,9,10,11,345)
  println(
    ls.map(_.toString * 2 toLong )
  )
}
