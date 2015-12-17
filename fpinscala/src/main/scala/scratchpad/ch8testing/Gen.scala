package scratchpad.ch8testing
import Gen._
import scratchpad.ch7parallelism.Par
import scratchpad.ch8testing.Prop._
import scratchpad.ch6state.{State,RNG}
import scratchpad.ch5laziness._
import Par._

case class Prop(run:(MaxSize,TestCases, RNG) => Result) {

  def &&(p: Prop): Prop = Prop { (max, tests, rng) =>
    run(max,tests,rng) match {
      case Passed | Proved => p.run(max,tests, rng)
      case res => res
    }
  }

  def ||(p: Prop): Prop = Prop {(max,tests, rng) =>
    run(max,tests,rng) match {
      case Falsified(f,s) => p.tag(f).run(max,tests,rng)
      case res => res
    }
  }

  def tag(msg:String)= Prop { (max,tests, rng) =>
    run(max,tests,rng) match {
      case Falsified(f, s) => Falsified(msg +"..\n"+ f,s)
      case res => res
    }
  }
}

object Prop {
  type TestCases = Int
  type MaxSize = Int
  type FailedCase = String
  type SuccessCount = Int


  sealed trait Result {
    def isFalsified: Boolean
  }

  case object Passed extends Result {
    def isFalsified = false
  }

  case class Falsified(failure: FailedCase,
                       successes: SuccessCount)
    extends Result {
    def isFalsified = true
  }

  case object Proved extends Result {
    def isFalsified = false
  }

  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
    Stream.unfold(rng)(rng => Some(g.sample.run(rng)))

  def forAll[A](g: SGen[A])(f: A => Boolean): Prop =
    forAll(g(_))(f)

  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop {
    (n,rng) => randomStream(as)(rng).zip(Stream.from(0)).take(n).map {
      case (a, i) => try {
        if (f(a)) Passed else Falsified(a.toString, i)
      } catch { case e: Exception => Falsified(buildMsg(a, e), i) }
    }.find(_.isFalsified).getOrElse(Passed)
  }

  def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop = Prop {
    (max,n,rng) =>
      val casesPerSize = (n + (max - 1)) / max
      val props: Stream[Prop] =
        Stream.from(0).take((n min max) + 1).map(i => forAll(g(i))(f))
      val prop: Prop =
        props.map(p => Prop { (max, _, rng) =>
          p.run(max, casesPerSize, rng)
        }).toList.reduce(_ && _)
      prop.run(max,n,rng)
  }



  def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
      s"generated an exception: ${e.getMessage}\n" +
      s"stack trace:\n ${e.getStackTrace.mkString("\n").take(5)}"


  def apply(f: (TestCases,RNG) => Result): Prop =
    Prop { (_,n,rng) => f(n,rng) }


  def run(p: Prop,
          maxSize: Int = 10,
          testCases: Int = 10,
          rng: RNG = RNG.Simple(System.currentTimeMillis)): Unit =
    p.run(maxSize, testCases, rng) match {
      case Falsified(msg, n) =>
        println(s"! Falsified after $n passed tests:\n $msg")
      case Passed =>
        println(s"+ OK, passed $testCases tests.")
      case Proved =>
        println(s"+ OK, proved property")
    }
  def check(p: => Boolean): Prop = Prop { (_,_,_) =>
    if (p) Passed else Falsified("()", 0)
  }
  def equal[A](p: Par[A], p2: Par[A]): Par[Boolean] =
    Par.map2(p,p2)(_ == _)

  val S = weighted(
    choose(1,4).map(java.util.concurrent.Executors.newFixedThreadPool) -> .75,
    Gen.unit(java.util.concurrent.Executors.newCachedThreadPool) -> .25) // `a -> b` is syntax sugar for `(a,b)`

  def forAllPar[A](g: Gen[A])(f: A => Par[Boolean]): Prop =
    forAll(S.map2(g)((_,_))) { case (s,a) => f(a)(s).get }

  def checkPar(p: Par[Boolean]): Prop =
    forAllPar(Gen.unit(()))(_ => p)

  def forAllPar2[A](g: Gen[A])(f: A => Par[Boolean]): Prop =
    forAll(S ** g) { case (s,a) => f(a)(s).get }

  def forAllPar3[A](g: Gen[A])(f: A => Par[Boolean]): Prop =
    forAll(S ** g) { case s ** a => f(a)(s).get }

}
case class Gen[+A](sample: State[RNG,A]){
  import Gen._

  def flatMap[B](f: A => Gen[B]):Gen[B] = Gen(sample.flatMap(f(_).sample))
  def map[B](f: A => B):Gen[B] = Gen(sample.map(f)) //flatMap(a => unit(f(a)))
  def listOfN(size: Gen[Int]):Gen[List[A]] =
    size.flatMap(s => Gen.listOfN(s,this))
  def unsized: SGen[A] = SGen(_ => this)
  def map2[B,C](b:Gen[B])(f:(A,B) => C):Gen[C] =
      Gen(sample.map2(b.sample)(f))
  def **[B](g: Gen[B]): Gen[(A,B)] =
    (this map2 g)((_,_))
}
object Gen {
  def unit[A](a: => A): Gen[A] = Gen(State.unit(a))

  def choose(start: Int, stopExclusive : Int):Gen[Int] =
    Gen(State(RNG.nonNegativeInt).map(i => start + i% (stopExclusive - start)))

  def boolean: Gen[Boolean] = Gen(State(RNG.bool))

  def listOfN[A](n:Int, g:Gen[A]) = Gen(State.sequence(List.fill(n)(g.sample)))

  def union[A](g1:Gen[A],g2:Gen[A]):Gen[A] = boolean.flatMap(if(_) g1 else g2)

  def weighted[A](g1:(Gen[A],Double), g2: (Gen[A],Double)):Gen[A] = {
    val(p1,p2) = (g1._2.abs, g2._2.abs)
    val p = p1 / (p1 + p2)
    for {
      d <- Gen(State(RNG.double2))
      r <- Gen(if(d >= p) g1._1.sample else g2._1.sample)
    } yield r

  }
  // Implement a listOf combinator that doesn’t accept an explicit size. It should return an
  // SGen instead of a Gen. The implementation should generate lists of the requested size.
  def listOf[A](g: Gen[A]): SGen[List[A]] = SGen(i => listOfN(i, g))
  //  Define listOf1 for generating nonempty lists, and then update your specification of
  //  max to use this generator.
  def listOf1[A](g: Gen[A]): SGen[List[A]] =
    SGen(n => Gen.listOfN(n + 1 max 1, g))

  def listOf1viaChoose[A](g:Gen[A]):Gen[List[A]] =
    for {
      i  <- choose(1,10)
      xs <- listOfN(i,g)
    } yield xs
}
case class SGen[+A](f: Int => Gen[A]){
  def apply(n: Int): Gen[A] = f(n)

  def flatMap[B](g: A => Gen[B]):SGen[B] =
    SGen(f andThen (_ flatMap g))

  def map[B](g: A => B):SGen[B] =
    SGen(f andThen(_ map g))

}
object ** {
  def unapply[A,B](p: (A,B)) = Some(p)
}

object testGens extends App {
  import Gen._
  import Prop._
  def rng = new RNG.Simple(scala.util.Random.nextLong())
  val g = Gen[Int](State(RNG.int))
  val gzero = unit(0)
  println(choose(-10000,10000).sample.run(rng))
  println(Gen.listOfN(10, choose(0,5)).sample.run(rng))
  println(weighted((gzero,5l),(g,1l)).sample.run(rng))
  val smallInt = choose(-12,12)
  val lists1 = Gen.listOf1(smallInt)
  val lists2 = Gen.listOf(smallInt)
  val maxProp:Prop = forAll(lists1) { ns =>
    //println(ns, ns.size)
    val max =  if (ns.nonEmpty) ns.max else 0
    !ns.exists(_ > max)
  }

  val sortedList:Prop = forAll(lists2){ xs =>
    def isSorted(ls:List[Int]):Boolean= ls match {
      case Nil | _::Nil => true
      case h::t => if (h > t.head ) false else isSorted(t)
    }
    isSorted(xs)
  }

  Prop.run(maxProp)
  Prop.run(sortedList)
  val ES = java.util.concurrent.Executors.newCachedThreadPool
  val p2 = Prop.check {
   val p = Par.map(Par.unit(1))(_ + 1)
   val p2 = Par.unit(2)
   p(ES).get() == p2(ES).get()
 }
  run(p2)

  val p3 = check{
    equal(
    Par.map(Par.unit(1))(_+ 1),
    Par.unit(2)
    )(ES).get
  }
 run(p3)

  val parint = Gen.choose(0,10) map (Par.unit(_))
  val p4 =
    forAllPar(parint)(n => equal(Par.map(n)(y => y), n))
  run(p4)

  lazy val otherInt:Gen[Par[Int]] = for {
    i  <- choose(1,100)
    xs  = Gen.listOfN(i,choose(1,Int.MaxValue))
    ys  = Gen.listOfN(i,choose(Int.MinValue, 0))
    xx <- Gen.weighted((xs,0.5),(ys,0.50))
  } yield Par.unit(xx.sum)

  for (_ <- 1 to 50) { println(otherInt.sample.run(rng)._1(ES).get)}

  val twProp = forAll(lists2) { xs =>
    val f:Int => Boolean = _ > 0
    (xs.takeWhile(f) ++ xs.dropWhile(f)) == xs
  }
  run(twProp)

  val forkLaw = forAllPar(otherInt)(pint => equal(Par.fork(pint), pint))

}