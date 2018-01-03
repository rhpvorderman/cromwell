package cwl

import common.validation.Validation._
import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.{FlatSpec, Matchers}
import wom.values.WomPopulatedFile

import scala.collection.JavaConverters._

class CwlJsEncoderSpec extends FlatSpec with Matchers with TableDrivenPropertyChecks {

  behavior of "CwlJsEncoder"

  it should "encode" in {
    val encoder = new CwlJsEncoder
    val file = WomPopulatedFile(
      value = "path/to/file.txt",
      checksumOption = None,
      sizeOption = None,
      formatOption = None,
      contentsOption = None,
      secondaryFiles = Nil
    )
    val expected = Map(
      "class" -> "File",
      "location" -> "path/to/file.txt",
      "path" -> "path/to/file.txt",
      "dirname" -> "path/to",
      "basename" -> "file.txt",
      "nameroot" -> "file",
      "nameext" -> ".txt"
    )
    val result = encoder.encode(file).toTry.get
    result should be(a[java.util.Map[_, _]])
    val resultMap = result.asInstanceOf[java.util.Map[String, AnyRef]].asScala
    resultMap.filterKeys(_ != "secondaryFiles") should contain theSameElementsAs expected
    resultMap("secondaryFiles") should be(a[Array[_]])
    resultMap("secondaryFiles").asInstanceOf[Array[_]] should be(empty)
  }

}
