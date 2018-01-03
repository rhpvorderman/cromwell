package wom.util

import cats.implicits._
import common.validation.ErrorOr._
import jdk.nashorn.api.scripting.ScriptObjectMirror
import wom.types._
import wom.values.{WomArray, WomBoolean, WomFloat, WomInteger, WomMap, WomOptionalValue, WomString, WomValue}

import scala.collection.JavaConverters._

/**
  * Converts a javascript value into a WomValue.
  *
  * Uses a lot of runtime pattern matching / partial functions.
  *
  * Perhaps a future version could use compile time checks with shapeless / polys.
  */
class JsDecoder {

  /**
    * Decodes a generic javascript type.
    */
  def decode(value: AnyRef): ErrorOr[WomValue] = {
    val partialDecoder =
      partialDecodePrimitive()
        .orElse(partialDecodeArrayWith(decodeArray))
        .orElse(partialDecodeFunctionInvalid())
        .orElse(partialDecodeMapWith(decodeMap))

    partialDecoder.applyOrElse[AnyRef, ErrorOr[WomValue]](
      value,
      _ => s"$getClass is unable to decode value: $value".invalidNel
    )
  }

  /**
    * Returns a partial function that can decode primitive values.
    */
  def partialDecodePrimitive(): PartialFunction[AnyRef, ErrorOr[WomValue]] = {
    case null => WomOptionalValue(WomNothingType, None).valid
    case string: String => WomString(string).valid
    case int: java.lang.Integer => WomInteger(int).valid
    case double: java.lang.Double if double == double.doubleValue.floor && !double.isInfinite =>
      // Nashorn javascript integers come back as doubles
      WomInteger(double.intValue).valid
    case double: java.lang.Double => WomFloat(double).valid
    case boolean: java.lang.Boolean => WomBoolean(boolean).valid
  }

  /**
    * Returns a partial function that can decode javascript or java arrays.
    */
  def partialDecodeArrayWith[A](arrayDecoder: Seq[AnyRef] => A): PartialFunction[AnyRef, A] = {
    case scriptObjectMirror: ScriptObjectMirror if scriptObjectMirror.isArray =>
      val array = (0 until scriptObjectMirror.size).map(scriptObjectMirror.getSlot)
      arrayDecoder(array)
    case javaArray: Array[_] =>
      val array = javaArray.map(_.asInstanceOf[AnyRef])
      arrayDecoder(array)
  }

  /**
    * Returns a partial function that detects javascript functions and produces a failure.
    */
  def partialDecodeFunctionInvalid(): PartialFunction[AnyRef, ErrorOr[WomValue]] = {
    case scriptObjectMirror: ScriptObjectMirror if scriptObjectMirror.isFunction =>
      s"$getClass is unable to decode function: $scriptObjectMirror".invalidNel
  }

  /**
    * Returns a partial function that can decode javascript or java maps.
    */
  def partialDecodeMapWith[A](mapDecoder: Map[String, AnyRef] => A): PartialFunction[AnyRef, A] = {
    case scriptObjectMirror: ScriptObjectMirror =>
      val map = scriptObjectMirror.getOwnKeys(true).map(key => key -> scriptObjectMirror.get(key)).toMap
      mapDecoder(map)
    case javaMap: java.util.Map[_, _] =>
      val map = javaMap.asScala.map({ case (key, value) => key.toString -> value.asInstanceOf[AnyRef] }).toMap
      mapDecoder(map)
  }

  /**
    * The type that should be returned for empty arrays or map-values.
    */
  def womEmptyType = WomNothingType

  /**
    * Returns the first type in the collection, or womEmptyType
    */
  def womValuesType(womValues: Iterable[WomValue]): WomType = {
    if (womValues.isEmpty) womEmptyType else womValues.head.womType
  }

  /**
    * Called to decode an array.
    */
  def decodeArray(array: Seq[AnyRef]): ErrorOr[WomValue] = {
    array.toList.traverse(decode).map(womValues =>
      WomArray(WomArrayType(womValuesType(womValues)), womValues)
    )
  }

  /**
    * Called to decode a map.
    */
  def decodeMap(map: Map[String, AnyRef]): ErrorOr[WomValue] = {
    map.traverse({
      case (key, value) =>
        val tuple: (ErrorOr[WomValue], ErrorOr[WomValue]) = (WomString(key).valid, decode(value))
        tuple.mapN((_, _))
    }).map({
      mapped =>
        val womValueType: WomType = womValuesType(mapped.values)
        WomMap(WomMapType(WomStringType, womValueType), mapped)
    })
  }
}
