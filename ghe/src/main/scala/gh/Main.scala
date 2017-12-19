package gh
import com.softwaremill.sttp._
import com.softwaremill.sttp.asynchttpclient.monix.AsyncHttpClientMonixBackend
import monix.execution.Scheduler.Implicits.global
import com.typesafe.config.ConfigFactory
import io.circe.parser._
import io.circe.optics.JsonPath._

object Main extends App {
  val config = ConfigFactory.load("secrets.conf")
  implicit val backend = AsyncHttpClientMonixBackend()
  val token = config.getString("token")

  val request = sttp.get(uri"https://api.github.com/orgs/monix/repos").auth.bearer(token)
  def qry(s:String) = sttp.get(uri"$s").auth.bearer(token)
  object repo {
    val url = root.each.url.string
    val name = root.each.name.string
  }

  for {
    r <- request.send()
    body <- r.body
    js <-  parse(body)
    urls = repo.url.getAll(js)
    names = repo.name.getAll(js)
  } {
    (names zip urls).toMap foreach println
    backend.close()
  }
}
