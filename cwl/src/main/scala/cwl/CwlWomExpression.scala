package cwl

import cats.syntax.option._
import cats.syntax.validated._
import common.validation.ErrorOr.ErrorOr
import common.validation.Validation._
import cwl.InitialWorkDirRequirement.IwdrListingArrayEntry
import cwl.WorkflowStepInput.InputSource
import cwl.command.ParentName
import wom.expression.{IoFunctionSet, WomExpression}
import wom.types._
import wom.values._

import scala.concurrent.Await
import scala.concurrent.duration.Duration

sealed trait CwlWomExpression extends WomExpression {

  def cwlExpressionType: WomType

  override def evaluateType(inputTypes: Map[String, WomType]): ErrorOr[WomType] = cwlExpressionType.validNel
}

case class JobPreparationExpression(expression: Expression,
                                    override val inputs: Set[String]) extends CwlWomExpression {
  val cwlExpressionType = WomAnyType

  override def sourceString = expression match {
    case Expression.ECMAScriptExpression(s) => s.value
    case Expression.ECMAScriptFunction(s) => s.value
  }

  override def evaluateValue(inputValues: Map[String, WomValue], ioFunctionSet: IoFunctionSet) = {
    val pc = ParameterContext(inputValues)
    expression.fold(EvaluateExpression).apply(pc)
  }

  override def evaluateFiles(inputTypes: Map[String, WomValue], ioFunctionSet: IoFunctionSet, coerceTo: WomType) = Set.empty[WomFile].validNel
}

case class CommandOutputExpression
(
  outputBinding: CommandOutputBinding,
  override val cwlExpressionType: WomType,
  override val inputs: Set[String],
  secondaryFilesCoproduct: Option[SecondaryFiles] = None,
  formatCoproduct: Option[StringOrExpression] = None //only valid when type: File
) extends CwlWomExpression {

  // TODO WOM: outputBinding.toString is probably not be the best representation of the outputBinding
  override def sourceString = outputBinding.toString

  // TODO: WOM: Can these also be wrapped in a WomOptional if the cwlExpressionType is '[null, File]'? Write a test and see what cromwell/salad produces
  override def evaluateValue(inputValues: Map[String, WomValue], ioFunctionSet: IoFunctionSet): ErrorOr[WomValue] = {
    outputBinding.generateOutputWomValue(
      inputValues,
      ioFunctionSet,
      cwlExpressionType,
      secondaryFilesCoproduct,
      formatCoproduct)
  }

  /**
    * Returns the list of files that _will be_ output after the command is run.
    *
    * In CWL, a list of outputs is specified as glob, say `*.bam`, plus a list of secondary files that may be in the
    * form of paths or specified using carets such as `^.bai`.
    *
    * The coerceTo may be one of four different values:
    * - WomSingleFileType
    * - WomArrayType(WomSingleFileType)
    * - WomSingleDirectoryType
    * - WomArrayType(WomSingleDirectoryType) (Possible according to the way the spec is written, but not likely?)
    */
  override def evaluateFiles(inputValues: Map[String, WomValue],
                             ioFunctionSet: IoFunctionSet,
                             coerceTo: WomType): ErrorOr[Set[WomFile]] = {
    outputBinding.primaryAndSecondaryFiles(inputValues, ioFunctionSet, coerceTo, secondaryFilesCoproduct).map(_.toSet)
  }
}

final case class WorkflowStepInputExpression(input: WorkflowStepInput, override val cwlExpressionType: WomType, graphInputs: Set[String])(implicit parentName: ParentName) extends CwlWomExpression {

  override def sourceString = input.toString

  override def evaluateValue(inputValues: Map[String, WomValue], ioFunctionSet: IoFunctionSet) = {
    (input.valueFrom, input.source) match {
      case (None, Some(WorkflowStepInputSource.String(id))) =>
        inputValues.
          get(FullyQualifiedName(id).id).
          toValidNel(s"could not find id $id in typeMap\n${inputValues.mkString("\n")}\nwhen evaluating $input.  Graph Inputs were ${graphInputs.mkString("\n")}")
      case _ => s"Could not do evaluateValue(${input.valueFrom}, ${input.source}), most likely it has not been implemented yet".invalidNel
    }
  }

  override def evaluateFiles(inputTypes: Map[String, WomValue], ioFunctionSet: IoFunctionSet, coerceTo: WomType) =
    "Programmer error: Shouldn't use WorkflowStepInputExpressions to find output files. You silly goose.".invalidNel

  override def inputs = graphInputs ++ input.source.toSet.flatMap{ inputSource: InputSource => inputSource match {
    case WorkflowStepInputSource.String(s) => Set(FullyQualifiedName(s).id)
    case WorkflowStepInputSource.StringArray(sa) => sa.map(FullyQualifiedName(_).id).toSet
  }}
}

final case class InitialWorkDirFileGeneratorExpression(entry: IwdrListingArrayEntry) extends CwlWomExpression {
  override def cwlExpressionType: WomType = WomSingleFileType
  override def sourceString: String = entry.toString

  override def evaluateValue(inputValues: Map[String, WomValue], ioFunctionSet: IoFunctionSet): ErrorOr[WomValue] = entry match {
    case IwdrListingArrayEntry.StringDirent(content, StringOrExpression.String(entryname), _) =>
      validate(Await.result(ioFunctionSet.writeFile(entryname, content), Duration.Inf))
    case _ => ??? // TODO WOM and the rest....
  }


  override def evaluateFiles(inputTypes: Map[String, WomValue], ioFunctionSet: IoFunctionSet, coerceTo: WomType): ErrorOr[Set[WomFile]] =
    "Programmer error: Shouldn't use InitialWorkDirRequirement listing to find output files. You silly goose.".invalidNel

  override def inputs: Set[String] = entry match {
    case IwdrListingArrayEntry.StringDirent(_, _, _) => Set.empty
    case _ => Set.empty // TODO WOM: For some cases this might need some...
  }
}
