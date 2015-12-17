
import java.util.UUID

import scala.util.{Try, Success, Failure}
import io.getquill._
import io.getquill.naming.SnakeCase

import scala.concurrent.ExecutionContext.Implicits.global


case class User(state: String, city: String, name: String, id: UUID)
case class SQLTest(id:Int, name:String)
case class SQLTestBig(id:Int,
name1:String,name2:String,name3:String,name4:String,name5:String,name6:String,name7:String,name8:String,
name9:String,name10:String,name11:String,name12:String,name13:String,name14:String,name15:String,name16:String,
name17:String,name18:String,name19:String,name20:String,name21:String,name22:String,name23:String,name24:String
)
object CassandraDrvTest {
  implicit val encodeUUID = mappedEncoding[UUID, String](_.toString)
  implicit val decodeUUID = mappedEncoding[String, UUID](UUID.fromString(_))


  def main(args: Array[String]) {
    lazy val cassie = source(new CassandraAsyncSourceConfig[SnakeCase]("cassie"))
    lazy val meesql = source(new MysqlAsyncSourceConfig[SnakeCase]("meesql"))

    val casQry = quote {
      query[User](_.entity("users"))
    }

    val sqlQry = quote {
      query[SQLTest](_.entity("test"))
    }

    val sqlQryBig = quote {
      query[SQLTestBig](_.entity("testbig"))
    }

    cassie.run(casQry)
      .andThen({
        case Success(v) => for (u <- v) println(u)
        case Failure(e) => println(e)
      })
      .andThen({ case _ => cassie.close() })

//    meesql.run(sqlQry)
//      .andThen({
//        case Success(v) => for (u <- v) println(u)
//        case Failure(e) => println(e)
//      })
//      .andThen({ case _ => meesql.close() })

    meesql.run(sqlQryBig)
      .andThen({
        case Success(v) => for (u <- v) println(u)
        case Failure(e) => println(e)
      })
      .andThen({ case _ => meesql.close() })

  }
}

