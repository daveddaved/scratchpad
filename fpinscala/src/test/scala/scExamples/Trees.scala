package scExamples

/**
 * Created by deema on 10/3/15.
 */
object Trees extends App{
  sealed abstract class Tree
  case class Node(left: Tree, right: Tree, v: Int) extends Tree
  case object Leaf extends Tree

  import org.scalacheck._
  import Gen._
  import Arbitrary.arbitrary

  val genLeaf = const(Leaf)

  val genNode = for {
    v <- arbitrary[Int]
    left <- Gen.lzy(genTree)
    right <- genTree
  } yield Node(left, right, v)

  def genTree: Gen[Tree] = oneOf(genLeaf, genNode)
  for (_<- 1 to 1000) { println(genTree.sample) }
}
