package scratchpad.ch2functions

object Functions {
  def fib(n: Int): BigInt = {
    def go(cur: Int, prev: BigInt, acc: BigInt): BigInt = {
      if (cur == 0) acc
      else go(cur - 1, acc, prev + acc)
    }
    go(n, 1, 0)
  }
  def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
    if(as.length <= 1) true
    else
      if (ordered(as(0), as(1))) isSorted(as.drop(1), ordered)
      else false
  }

  /**
  * EXERCISE 2.3
  * Let’s look at another example, currying, 9 which converts a function f of two arguments
  * into a function of one argument that partially applies f . Here again there’s only one
  * implementation that compiles. Write this implementation
   * */
  def curry[A,B,C](f: (A, B) => C): A => (B => C) = (a:A) => (b:B) => f(a,b)

  /**
  * EXERCISE 2.4
  * Implement uncurry , which reverses the transformation of curry . Note that since =>
  * associates to the right, A => (B => C) can be written as A => B => C
   * */
  def uncurry[A,B,C](f: A => B => C): (A, B) => C = (a:A, b:B) => f(a)(b)

  /**
   * EXERCISE 2.5
   * Implement the higher-order function that composes two functions.
   */
  def compose[A,B,C](f: B => C, g: A => B): A => C = (a:A) => f(g(a))
}

object testF extends App {
  for (i <- 0 to 12) {println(Functions.fib(i))}

  val testArrs = Array(Array(), Array(1,2), Array(1,2,3),Array(1,2,3,4,5), Array(1), Array(0,1), Array(1,0), Array(2,1,3,4))
  testArrs.foreach(
  arr => println(arr.mkString(","), Functions.isSorted(arr, (x:Int,y:Int) => x < y))
  )
}
