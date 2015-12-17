package scratchpad.ch6state




trait RNG {
  def nextInt: (Int, RNG)
}
object RNG {

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      val nextRNG = Simple(newSeed)
      val n = (newSeed >>> 16).toInt
      (n, nextRNG)
    }
  }

  def randomPair(rng: RNG): ((Int,Int), RNG) = {
    val (firstInt, secondRng) = rng.nextInt
    val (secondInt, finalRng) = secondRng.nextInt
    ((firstInt,secondInt), finalRng)
  }

  /**
   * Write a function that uses RNG.nextInt to generate a random integer between 0 and
   * Int.maxValue (inclusive). Make sure to handle the corner case when nextInt returns
   * Int.MinValue , which doesn’t have a non-negative counterpart.
   */
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (int, nextRng) = rng.nextInt
    if (int == Int.MinValue) nonNegativeInt(rng)
    else (math.abs(int),nextRng)
  }

  /**
   * Write a function to generate a Double between 0 and 1 , not including 1 .
   * Note: You can use Int.MaxValue to obtain the maximum positive integer value,
   * and you can use x.toDouble to convert an x: Int to a Double .
   */
  private def pair2Double(i1:Int,i2:Int):Double = {
    math.abs((i1.toDouble / i2.toDouble) %1 )
  }
  def double(rng: RNG): (Double, RNG) = {
    val ((int1,int2), r) = randomPair(rng)
    if (int1 == int2 || int2 == 0) double(r)
      else (pair2Double(int1,int2),r)
  }


  /**
   * Write functions to generate an (Int, Double) pair, a (Double, Int) pair, and a
   * (Double, Double, Double) 3-tuple. You should be able to reuse the functions you’ve
   * already written.
   */
  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (int,r) = rng.nextInt
    val (dbl, finalR) = double(r)
    ((int,dbl),finalR)

  }
  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val (dbl,r) = double(rng)
    val (int, finalR) = r.nextInt
    ((dbl,int),finalR)
  }
  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (d1,r2) = double(rng)
    val (d2,r3) = double(r2)
    val (d3,r4) = double(r3)
    ((d1,d2,d3), r4)
  }
  def boolean(rng: RNG): (Boolean, RNG) =
    rng.nextInt match { case (i,rng2) => (i%2==0,rng2) }

  /**
   * Write a function to generate a list of random integers.
   */
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    def go(acc:List[Int], cur:Int, curR:RNG): (List[Int], RNG) = {
      if (cur == 0) (acc,curR)
      else {
        val (c, nextR) = curR.nextInt
        go(c :: acc, cur - 1, nextR)
      }
    }
    go(Nil,count,rng)
  }
  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt
  val bool: Rand[Boolean] = map(int)(_ % 2 == 0)

  val _nonNegativeInt:Rand[Int] =
    map(int){ i => if(i == Int.MinValue) math.abs(i+1) else math.abs(i) }

  def unit[A](a: A): Rand[A] = rng => (a, rng)

  def map1[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeEven: Rand[Int] =
    map(nonNegativeInt)(i => i - i % 2)

  /**
   * Use map to reimplement double in a more elegant way. See exercise 6.2.
   */
  def double2: Rand[Double] =
    map(randomPair) {
      case (i1,i2) if i1 == i2 || i2 == 0 => pair2Double(i1,i2 +1)
      case (i1,i2) => pair2Double(i1,i2)
    }

  /**
   * Write the implementation of map2 based on the following signature. This function
   * takes two actions, ra and rb , and a function f for combining their results, and returns
   * a new action that combines them:
   */
  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng => {
      val (a,r) =  ra(rng)
      val (b,r1) = rb(r)
      (f(a,b),r1)
    }

  def both[A,B](ra: Rand[A], rb: Rand[B]): Rand[(A,B)] =
    map2(ra, rb)((_, _))

  val randIntDouble: Rand[(Int, Double)] =
    both(int, double)

  val randDoubleInt: Rand[(Double, Int)] =
    both(double, int)
  /**
   * Hard: If you can combine two RNG transitions, you should be able to combine a whole
   * list of them. Implement sequence for combining a List of transitions into a single
   * transition.
   * Use it to reimplement the ints function you wrote before. For the latter,
   * you can use the standard library function List.fill(n)(x) to make a list with x
   * repeated n times.
   */
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs match {
      case Nil => unit[List[A]](Nil)
      case h :: t  => map2(h, sequence(t))(_::_)
    }
  def _sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit[List[A]](Nil)){(h,t) => map2(h,t)(_::_)}

  def ints2(count: Int): Rand[List[Int]] =
    _sequence(List.fill(count)(int))
/**
  * Implement flatMap , and then use it to implement nonNegativeLessThan .
  */

def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = { rng =>
   val (a,r) = f(rng)
   g(a)(r)
 }

def nonNegativeLessThan(n:Int):Rand[Int] =
  flatMap(_nonNegativeInt) { i =>
    val mod = i % n
    if (i + (n-1) - mod >= 0) unit(mod) else nonNegativeLessThan(n)
  }

  /**
   * Reimplement map and map2 in terms of flatMap . The fact that this is possible is what
   * we’re referring to when we say that flatMap is more powerful than map and map2 .
   */
  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s) { a => unit(f(a)) }

  def _map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
     flatMap(ra) { a => map(rb) {b => f(a,b)} }

}

/**
 * Generalize the functions unit , map , map2 , flatMap , and sequence .
 * Add them as methods on the State case class where possible.
 * Otherwise you should put them in a State companion object.
 */
case class State[S,+A](run: S => (A,S)) {
  import State._

  def flatMap[B](g: A => State[S,B]):State[S,B] = State(s => {
    val (a,next) = run(s)
    g(a).run(next)
   }
  )

  def map[B](f: A => B):State[S,B] =
   flatMap( a => unit(f(a)))

  def map2[B,C](sb: State[S,B])(f: (A, B) => C): State[S,C] =
    for {
    a <- this
    b <- sb
  } yield f(a,b)


}
object State {
  type Rand[A] = State[RNG, A]

  def unit[S,A](a: A): State[S,A] = State(s => (a,s))
  def sequence[S,A](fs: List[State[S,A]]): State[S,List[A]] =
    fs.reverse.foldLeft(unit[S,List[A]](List[A]())){(l, r) =>
        r.map2(l) { _ :: _ }
    }

  def get[S]:State[S,S] = State(s => (s,s))
  def set[S](s:S):State[S,Unit] = State(_ => ((),s))
  def modify[S](f: S=>S):State[S,Unit] = for {
    s <- get
    _<- set(f(s))
  } yield ()
 // def testList = State.sequence(List.fill(10)(unit(1)))
}

/**
 * Hard: To gain experience with the use of State, implement a finite state automaton
 * that models a simple candy dispenser. The machine has two types of input: you can
 * insert a coin, or you can turn the knob to dispense candy. It can be in one of two
 * states: locked or unlocked. It also tracks how many candies are left and how many
 * coins it contains.
 * The rules of the machine are as follows:
 * - Inserting a coin into a locked machine will cause it to unlock if there’s any
 *   candy left.
 * - Turning the knob on an unlocked machine will cause it to dispense candy and
 *   become locked.
 * - Turning the knob on a locked machine or inserting a coin into an unlocked
 *   machine does nothing.
 * - A machine that’s out of candy ignores all inputs.
 * The method simulateMachine should operate the machine based on the list of inputs
 * and return the number of coins and candies left in the machine at the end. For example,
 * if the input Machine has 10 coins and 5 candies, and a total of 4 candies are successfully
 * bought, the output should be (14, 1).
 */


sealed trait Input
case object Coin extends Input
case object Turn extends Input
case class Machine(locked:Boolean, candies:Int, coins: Int)


object testMachine extends App {
  import State._
  val start = Machine(locked = true,12,5)
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = {
    def rules(in: Input)(m: Machine) = {
      (in, m) match {
        case (Coin, Machine(true, cn, _)) if cn > 0 => m.copy(locked = false)
        case (Turn, Machine(false, cn, mn)) => m.copy(locked = true, cn - 1, mn + 1)
        case _ => m
      }
    }
    val transition = modify[Machine] _ compose rules
    val states = sequence(inputs.map(transition))
    states.flatMap(_=> get).map(m => (m.candies,m.coins))
  }
  println(simulateMachine(List(Coin,Turn,Coin,Turn,Coin)).run(start))
}
object testState extends App {
  import State._
  import RNG._
  val integers = ints2(10)(Simple(10l))
  val integers1 = ints(10)(Simple(10l))
  println(integers)
  println(integers1)
  val ls = State.unit(1).run(Simple(10))
  val lsx = (1 to 5).toList.map(State.unit[RNG,Int](_))
  println(lsx.map(_.run(Simple(10l))))
  println(State.sequence(lsx).run(Simple(10l)))

}