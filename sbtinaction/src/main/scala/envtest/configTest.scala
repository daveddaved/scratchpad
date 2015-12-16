package envtest


import com.typesafe.config.{ConfigFactory,Config}
import scala.collection.JavaConversions._

case class User(name:String,id:Int)
case class Server(name:String, url:Option[String])

trait ConfigReader[T] {
  def read(path:Option[String])(implicit c:Config):Option[T]
  def readMany(path:String)(implicit c:Config):List[Option[T]] =
     c.getConfigList(path).toList map (c1 => read(None)(c1))
}
object ConfigReader {
  implicit object UserReader extends ConfigReader[User] {
    def read(path:Option[String])(implicit c:Config):Option[User] = {
     path match {
       case None => Some(User(c.getString("name"), c.getInt("id")))
       case Some(p) => read(None)(c.getConfig(p))
     }
    }
  }
}

object configLoader {

  private val config = ConfigFactory.load()
  implicit val base = config.getConfig(config.getString("target"))
  def readOne[T:ConfigReader](path:Option[String]) = implicitly[ConfigReader[T]].read(path).get
  def readMany[T:ConfigReader](path:String) = implicitly[ConfigReader[T]].readMany(path).flatten
  def get(path:String)(implicit cn:Config) = cn.getConfig(path)
}

object configTest extends App {
  import configLoader._
  //import ConfigReader._


  println(sys.env.getOrElse("TARGET","Not Set"))
  println(readOne[User](Some("user1")))
  println(readMany[User]("users"))
//  val (int,str) = (base.getInt("int"), base.getString("string"))
//  println(s"Env: $int, $str")
//  println(readOne[User](Some("user1")))
}
