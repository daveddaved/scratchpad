import io.getquill.naming.SnakeCase
import io.getquill.sources.async.AsyncSource
import io.getquill.sources.sql.idiom.MySQLDialect
import com.github.mauricio.async.db.Connection

package object db {

  type AsyncDatabase = AsyncSource[MySQLDialect, SnakeCase, Connection]

}