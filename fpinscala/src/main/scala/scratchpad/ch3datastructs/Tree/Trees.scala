package scratchpad.ch3datastructs.Tree

trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
object Tree {
  /**
   * Write a function size that counts the number of nodes (leaves and branches) in a tree.
   */
  def size[A](t:Tree[A]):Int =  t match {
    case Leaf(_) => 1
    case Branch(l,r) => 1 + size(l) + size(r)
  }

  /**
   * Write a function maximum that returns the maximum element in a Tree[Int].
   * (Note: In Scala, you can use x.max(y) or x max y to compute the maximum of two integers x andy.)
   */
  def maximum[A](t:Tree[A])(mx:(A,A)=>A):A = t match {
    case Leaf(a) => a
    case Branch(l,r) => mx(maximum(l)(mx) , maximum(r)(mx))
  }
  /**
   * Write a function depth that returns the maximum path length from the root of a tree to any leaf.
   */
  def depth[A](t:Tree[A]):Int = t match {
    case Leaf(_) => 0
    case Branch(l,r) => 1 + (depth(l) max depth(r))
  }

  /**
   * Write a function map, analogous to the method of the same name on List, that modi- fies each element in a tree with a given function.
   */
  def map[A,B](t:Tree[A])(f:A=>B):Tree[B] = t match {
    case Leaf(l) => Leaf(f(l))
    case Branch(l,r) => Branch(map(l)(f),map(r)(f) )
  }

  /**
   * Generalize size, maximum, depth, and map, writing a new function fold that abstracts over their similarities.
   * Reimplement them in terms of this more general function. Can you draw an analogy between this fold function
   * and the left and right folds for List?
   */
  def fold[A,B](t:Tree[A], z:A=>B)(f:(B,B) =>B):B = t match {
    case Leaf(a) => z(a)
    case Branch(l,r) => f( fold(l,z)(f), fold(r,z)(f) )
  }
  def mapViaFold[A,B](t:Tree[A])(f:A=>B):Tree[B] = fold(t, (a:A)=> Leaf(f(a)):Tree[B]) { (l,r) => Branch(l,r) }
}


object Trees extends App{
    val l1:Tree[Int] = Leaf(10)

    val t1:Tree[Int] = Branch(l1, Branch(Leaf(2),Branch(Branch(Leaf(3),Leaf(2)),Leaf(0))))
    println("Max: "+ Tree.maximum(t1)((a:Int,b:Int) => a.max(b)))
    println("Size: "+ Tree.size(t1))
  println("Depth:" + Tree.depth(t1))
  println("Map: " + Tree.map(t1)( i => s"got a $i")  )
  println("Size via fold " + Tree.fold(t1, (i:Int) => 1){(l,r) => 1 + l  + r } )
  println("Depth via fold " + Tree.fold(t1, (i:Int) => 0){(l,r) => 1 + (l max r) } )
  println("Max via fold " + Tree.fold(t1, (i:Int) => i){(l,r) => l max r } )
  println("Map via fold " + Tree.mapViaFold(t1) {i => s"got a $i" }    )
  println(Tree.depth(Leaf(1)))
}
