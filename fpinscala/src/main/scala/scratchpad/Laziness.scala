package scratchpad

object Laziness extends App {
  def maybeTwice(b:Boolean,i: => Int ) = {
    if(b) i + i else 0
  }
  def maybeTwice2(b:Boolean,i: => Int ) = {
    lazy val cache = i
    if(b) cache + cache else 0
  }
  println(maybeTwice(true, { println("Hi1") ; 2}), maybeTwice2(true, { println("Hi2") ;2 }))
}
