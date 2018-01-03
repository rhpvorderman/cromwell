package wom.util

import cats.data.Validated
import cats.implicits._
import common.validation.ErrorOr._
import wom.types.WomNothingType
import wom.values.{WomArray, WomBoolean, WomFloat, WomInteger, WomMap, WomOptionalValue, WomString, WomValue}

import scala.collection.JavaConverters._

/**
  * Converts a WomValue into a javascript compatible value.
  */
class JsEncoder {

  /**
    * Base implementation converts any WomPrimitive (except WomFile) into a javascript compatible value.
    *
    * Inputs, and returned output must be one of:
    * - WomString
    * - WomBoolean
    * - WomFloat
    * - WomInteger
    * - WomMap
    * - WomArray
    * - A "WomNull" equal to WomOptionalValue(WomNothingType, None)
    *
    * The WomMap keys and values, and WomArray elements must be the one of the above, recursively.
    *
    * WomFile are not permitted, and must be already converted to one of the above types.
    *
    * @param value A WOM value.
    * @return The javascript equivalent.
    */
  def encode(value: WomValue): ErrorOr[AnyRef] = {
    value match {
      case WomOptionalValue(WomNothingType, None) => Validated.Valid(null)
      case WomString(string) => string.valid
      case WomInteger(int) => int.asInstanceOf[java.lang.Integer].valid
      case WomFloat(double) => double.asInstanceOf[java.lang.Double].valid
      case WomBoolean(boolean) => boolean.asInstanceOf[java.lang.Boolean].valid
      case WomArray(_, array) => array.toList.traverse[ErrorOr, AnyRef](encode).map(_.toArray)
      case WomMap(_, map) => map.traverse({
        case (mapKey, mapValue) => (encode(mapKey), encode(mapValue)).mapN((_, _))
      }).map(_.asJava)
      case _ => s"$getClass is unable to encode value: $value".invalidNel
    }
  }
}
