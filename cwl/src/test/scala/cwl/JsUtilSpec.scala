package cwl

import org.scalatest.{FlatSpec, Matchers}
import wom.values._
import collection.JavaConverters._

class JsUtilSpec extends FlatSpec with Matchers {

  behavior of "JsUtil"

  it should "eval" in {
    val values = Map(
      "myName" -> Map(true ->  Array("myValue")).asJava.asInstanceOf[AnyRef]
    ).asJava

    val expr = "myName[true][0] + 'Plus'"

    val result: WomValue = JsUtil.eval(expr, values)

    result should be(WomString("myValuePlus"))
  }

}
