package models

import play.api.libs.json.{ JsError, JsResult, Json }

/**
 * Created by deema on 7/18/16.
 */
case class User(name: String, id: Long)
object User {
  implicit val jfmt = Json.format[User]
}

object testJsfmt extends App {

  val user = User("First", Long.MaxValue)

  println(Json.toJson(user).toString())

  val js = """ {"name":"First","id":"L9223372036854775807"} """

  val u1 = Json.fromJson[User](Json.parse(js)) orElse { JsError() }
  println(u1)
}