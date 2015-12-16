package scratchpad
import org.scalacheck._
import scratchpad.ch3datastructs.Tree.{Branch, Leaf, Tree}


object TreeSpec extends Properties("Trees") {

  import Gen._
  import Prop._
  import Arbitrary._

  val genLeaf:Gen[Leaf[Int]] = for {
    i <- Arbitrary.arbitrary[Int]
  } yield Leaf(i)

 def genBranch(size:Int):Gen[Branch[Int]] = for {
    left <- Gen.lzy(genTree(size -1))
    right <-  genTree(size -1)
  } yield Branch(left, right)

  def genTree(maxDepth:Int):Gen[Tree[Int]] = if (maxDepth > 0) oneOf(genLeaf,genBranch(maxDepth)) else genLeaf

  lazy val genSizedTrees:Gen[(Tree[Int],Int)] = for {
    depth <- choose(1,1000)
    tree <- genTree(depth) suchThat(Tree.size(_) > 10)
  } yield (tree, depth)

  property("depth cannot exceed max depth") = forAll(genSizedTrees) {case (tree,maxDepth) =>
    Tree.depth(tree) <= maxDepth
  }


}
