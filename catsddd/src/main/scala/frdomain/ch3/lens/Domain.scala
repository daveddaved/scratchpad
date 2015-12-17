package frdomain.ch3
package lens
import shapeless._
case class Address(no: String, street: String, city: String, state: String, zip: String)
case class Customer(id: Int, name: String, address: Address)

trait AddressLenses {
  protected val noLens = lens[Address] >> 'no
  //    Lens[Address, String](
  //    get = _.no,
  //    set = (o, v) => o.copy(no = v)
  //  )
  protected val streetLens = lens[Address] >> 'street
//    Lens[Address, String](
//    get = _.street,
//    set = (o, v) => o.copy(street = v)
//  )

  protected val cityLens = lens[Address] >> 'city
//    Lens[Address, String](
//    get = _.city,
//    set = (o, v) => o.copy(city = v)
//  )

  protected val stateLens = lens[Address] >> 'state
//    Lens[Address, String](
//    get = _.state,
//    set = (o, v) => o.copy(state = v)
//  )

  protected val zipLens = lens[Address] >> 'zip
//    Lens[Address, String](
//    get = _.zip,
//    set = (o, v) => o.copy(zip = v)
//  )
}

trait CustomerLenses {
//  protected val naLens = lens[Customer] >> 'id

  protected val nameLens = lens[Customer] >> 'name
//    Lens[Customer, Int](
//    get = _.id,
//    set = (o, v) => o.copy(id = v)
//  )

  protected val addressLens = lens[Customer]
//    Lens[Customer, Address](
//    get = _.address,
//    set = (o, v) => o.copy(address = v)
//  )
  protected val czip = lens[Customer].address.zip


  val composed = lens[Customer] >> 'address >> 'zip
}
object testLens extends App with AddressLenses with CustomerLenses {
//  import Lens._
  val a = Address(no = "B-12", street = "Monroe Street",
  city = "Denver", state = "CO", zip = "80231")

  val c = Customer(12, "John D Cook", a)
  implicit val zl = zipLens
  implicit  val custAddrNoLens = czip
  val c1 = custAddrNoLens.get(c)
  val c2 = custAddrNoLens.set(c)("123")
  val c3 = composed.get(c)
  val c4 = composed.set(c)("9999")
  println("czip: " + c1)
    //set("B675")
  println("c1 set 123: " + c2)
  println("composed: " + c3)
  println("composed set 999: "+ c4)
}