package wom.types

import spray.json.JsString
import wom.values._

import scala.util.{Success, Try}

sealed trait WomFileType extends WomType {
  override def toDisplayString: String = s"$getClass"

  override protected def coercion: PartialFunction[Any, WomFile] = PartialFunction.empty
}

sealed trait WomPrimitiveFileType extends WomFileType with WomPrimitiveType

case object WomUnlistedDirectoryType extends WomPrimitiveFileType {
  override val toDisplayString: String = "Directory"

  override protected def coercion: PartialFunction[Any, WomUnlistedDirectory] = {
    case s: String => WomUnlistedDirectory(s)
    case s: JsString => WomUnlistedDirectory(s.value)
    case s: WomString => WomUnlistedDirectory(s.valueString)
    case d: WomUnlistedDirectory => d
  }

  override def equals(rhs: WomType): Try[WomType] = rhs match {
    case WomUnlistedDirectoryType => Success(WomBooleanType)
    case WomStringType => Success(WomBooleanType)
    case WomOptionalType(memberType) => equals(memberType)
    case _ => invalid(s"$this == $rhs")
  }

  override def add(rhs: WomType): Try[WomType] = rhs match {
    case WomStringType => Success(WomUnlistedDirectoryType)
    case WomOptionalType(memberType) => add(memberType)
    case _ => invalid(s"$this + $rhs")
  }
}

case object WomSingleFileType extends WomPrimitiveFileType {
  override val toDisplayString: String = "File"

  override protected def coercion: PartialFunction[Any, WomSingleFile] = {
    case s: String => WomSingleFile(s)
    case s: JsString => WomSingleFile(s.value)
    case s: WomString => WomSingleFile(s.valueString)
    case f: WomSingleFile => f
  }

  override def equals(rhs: WomType): Try[WomType] = rhs match {
    case WomSingleFileType => Success(WomBooleanType)
    case WomStringType => Success(WomBooleanType)
    case WomOptionalType(memberType) => equals(memberType)
    case _ => invalid(s"$this == $rhs")
  }

  override def add(rhs: WomType): Try[WomType] = rhs match {
    case WomStringType => Success(WomSingleFileType)
    case WomOptionalType(memberType) => add(memberType)
    case _ => invalid(s"$this + $rhs")
  }
}

case object WomGlobFileType extends WomPrimitiveFileType {
  override val toDisplayString: String = "Glob"

  override def equals(rhs: WomType): Try[WomType] = rhs match {
    case WomGlobFileType => Success(WomBooleanType)
    case WomStringType => Success(WomBooleanType)
    case WomOptionalType(memberType) => equals(memberType)
    case _ => invalid(s"$this == $rhs")
  }

  override protected def coercion: PartialFunction[Any, WomGlobFile] = {
    case s: String => WomGlobFile(s)
    case s: JsString => WomGlobFile(s.value)
    case s: WomString => WomGlobFile(s.valueString)
    case f: WomGlobFile => f
  }

  override def add(rhs: WomType): Try[WomType] = rhs match {
    case WomStringType => Success(WomGlobFileType)
    case WomOptionalType(memberType) => add(memberType)
    case _ => invalid(s"$this + $rhs")
  }
}

case object WomListedDirectoryType extends WomFileType

case object WomListedLiteralType extends WomFileType

case object WomPrimaryFileType extends WomFileType

case object WomContentLiteralType extends WomFileType

case object WomPopulatedFileType extends WomFileType
