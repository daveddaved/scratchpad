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

