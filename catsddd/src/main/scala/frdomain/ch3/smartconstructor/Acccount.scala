package frdomain.ch3.smartconstructor

import java.util.{Calendar, Date}

import scala.util.{Failure, Success, Try}

object common {
  type Amount = BigDecimal

  def today = Calendar.getInstance.getTime
}

import frdomain.ch3.smartconstructor.common._

case class Balance(amount: Amount = 0)

sealed trait Account {
  def no: String
  def name: String
  def dateOfOpen: Option[Date]
  def dateOfClose: Option[Date]
  def balance: Balance
}

final case class CheckingAccount(no: String,
                                 name: String,
                                 dateOfOpen: Option[Date],
                                 dateOfClose: Option[Date] = None,
                                 balance: Balance = Balance())
    extends Account

final case class SavingsAccount(no: String,
                                name: String,
                                rateOfInterest: Amount,
                                dateOfOpen: Option[Date],
                                dateOfClose: Option[Date] = None,
                                balance: Balance = Balance())
    extends Account

object Account {
  def checkingAccount(no: String,
                      name: String,
                      openDate: Option[Date],
                      closeDate: Option[Date],
                      balance: Balance): Try[Account] = {

    closeDateCheck(openDate, closeDate).map { d =>
      CheckingAccount(no, name, Some(d._1), d._2, balance)
    }
  }

  def savingsAccount(no: String,
                     name: String,
                     rate: BigDecimal,
                     openDate: Option[Date],
                     closeDate: Option[Date],
                     balance: Balance): Try[Account] = {

    closeDateCheck(openDate, closeDate).map { d =>
      if (rate <= BigDecimal(0))
        throw new Exception(s"Interest rate $rate must be > 0")
      else
        SavingsAccount(no, name, rate, Some(d._1), d._2, balance)
    }
  }

  private def closeDateCheck(
      openDate: Option[Date],
      closeDate: Option[Date]): Try[(Date, Option[Date])] = {
    val od = openDate.getOrElse(today)

    closeDate.map { cd =>
      if (cd before od)
        Failure(new Exception(
                s"Close date [$cd] cannot be earlier than open date [$od]"))
      else Success((od, Some(cd)))
    }.getOrElse {
      Success((od, closeDate))
    }
  }
}
object testCompose extends App {
  def prepend[I](xs:List[I],i:I):List[I] = i :: xs
  def append[I](xs:List[I],i:I):List[I] = xs :+ i

  def crd[I](i:I): List[I]  => List[I] =
    { xs:List[I] =>
      val g = (prepend[I] _).curried(xs)(_)
      val f = (append[I] _).curried(_:List[I])(i)
      (f compose g)(i)
    }

  println(crd("a")(List("d","g")))

}
object TestPb extends App {
  import person._
  import Person._
  import BankAccount._
  import com.trueaccord.lenses.Lens
  val p = Person(
    firstName = Some("first"),
    lastName = Some("last"),
    age = Some(10),
    address = Some(Address(street = Some("street"),city = Some("city"))),
    bankAccounts = Vector(BankAccount(Some("route"), Some("number")))
  )
  val ageLens = Lens[Person,Int](_.age.getOrElse(0))((p:Person,age:Int) => p.copy(age = Some(age)))
//  ageLens
  val p2 = p.update(_.address.street := "New Street", _.firstName := "ffff" )
  val l3 =(i:Int) => Person().update(_.age := i)
//  val ls:Lens[Person,Person] = Lens[Person,Person](_.update(_.age := 11))
  val l1 =  Person().update(_.age := 1)
  val cp = Person().update(_.age := 1, _.address.street := "asd")
  println(cp)
  println(p2)
}
