package envtest

import org.scalatest.{FlatSpec, Matchers}

/**
 * Created by deema on 11/2/15.
 */
class ConfigSpec extends FlatSpec with Matchers {
  "Config" should "load" in {
    alert("" + configLoader.base)
    configLoader.base.getString("string") should be ("this is dev")
  }
}
