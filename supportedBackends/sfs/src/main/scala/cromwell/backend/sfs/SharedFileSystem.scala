package cromwell.backend.sfs

import java.io.{FileNotFoundException, IOException}

import akka.actor.ActorContext
import akka.stream.ActorMaterializer
import cats.instances.try_._
import cats.syntax.functor._
import com.typesafe.config.Config
import com.typesafe.scalalogging.StrictLogging
import common.collections.EnhancedCollections._
import common.util.TryUtil
import cromwell.backend.io.JobPaths
import cromwell.core.CromwellFatalExceptionMarker
import cromwell.core.path.{DefaultPath, DefaultPathBuilder, Path, PathFactory}
import cromwell.filesystems.http.HttpPathBuilder
import wom.WomFileMapper
import wom.values._

import scala.collection.JavaConverters._
import scala.concurrent.{Await}
import scala.concurrent.duration.Duration
import scala.language.postfixOps
import scala.util.{Failure, Success, Try}

object SharedFileSystem extends StrictLogging {

  final case class AttemptedLookupResult(name: String, value: Try[WomValue]) {
    def toPair: (String, Try[WomValue]) = name -> value
  }

  object AttemptedLookupResult {
    implicit class AugmentedAttemptedLookupSequence(s: Seq[AttemptedLookupResult]) {
      def toLookupMap: Map[String, WomValue] = s collect {
        case AttemptedLookupResult(name, Success(value)) => (name, value)
      } toMap
    }
  }

  case class PairOfFiles(src: Path, dst: Path)
  type DuplicationStrategy = (Path, Path, Boolean) => Try[Unit]
  
  private def createParentDirectory(executionPath: Path, docker: Boolean) = {
    if (docker) executionPath.parent.createPermissionedDirectories()
    else executionPath.parent.createDirectories()
  }

  /**
    * Return a `Success` result if the file has already been localized, otherwise `Failure`.
    */
  private def localizePathAlreadyLocalized(originalPath: Path, executionPath: Path, docker: Boolean): Try[Unit] = {
    if (executionPath.exists) Success(()) else Failure(new RuntimeException(s"$originalPath doesn't exist"))
  }

  private def localizePathViaCopy(originalPath: Path, executionPath: Path, docker: Boolean): Try[Unit] = {
    val action = Try {
      createParentDirectory(executionPath, docker)
      val executionTmpPath = executionPath.plusExt("tmp")
      originalPath.copyTo(executionTmpPath, overwrite = true).moveTo(executionPath, overwrite = true)
    }.void
    logOnFailure(action, "copy")
  }

  private def localizePathViaHardLink(originalPath: Path, executionPath: Path, docker: Boolean): Try[Unit] = {
    val action = Try {
      createParentDirectory(executionPath, docker)
      originalPath.linkTo(executionPath)
    }.void
    logOnFailure(action, "hard link")
  }

  private def localizePathViaSymbolicLink(originalPath: Path, executionPath: Path, docker: Boolean): Try[Unit] = {
      if (originalPath.isDirectory) Failure(new UnsupportedOperationException("Cannot localize directory with symbolic links"))
      else if (!originalPath.exists) Failure(new FileNotFoundException(originalPath.pathAsString))
      else {
        val action = Try {
          createParentDirectory(executionPath, docker)
          executionPath.linkTo(originalPath, symbolic = true)
        }.void
        logOnFailure(action, "symbolic link")
      }
  }

  private def logOnFailure(action: Try[Unit], actionLabel: String): Try[Unit] = {
    if (action.isFailure) logger.warn(s"Localization via $actionLabel has failed: ${action.failed.get.getMessage}")
    action
  }

  private def duplicate(description: String, source: Path, dest: Path, strategies: Stream[DuplicationStrategy], docker: Boolean): Try[Unit] = {
    val attempts: Stream[Try[Unit]] = strategies.map(_.apply(source.followSymbolicLinks, dest, docker))
    attempts.find(_.isSuccess) getOrElse {
      TryUtil.sequence(attempts, s"Could not $description $source -> $dest").void
    }
  }
}

trait SharedFileSystem extends PathFactory {
  import SharedFileSystem._

  def sharedFileSystemConfig: Config

  lazy val cachedCopyDir: Option[Path] = None

  private def localizePathViaCachedCopy(originalPath: Path, executionPath: Path, docker: Boolean): Try[Unit] = {
    val action = Try {
      createParentDirectory(executionPath, docker)
      // Hash the parent. This will make sure bamfiles and their indexes stay in the same dir. This is not ideal. But should work.
      // There should be no file collisions because two files with the same name cannot exist in the same parent dir.
      // use .get . This strategy should not be used when there is no cachedCopyDir
      val cachedCopySubDir: Path = cachedCopyDir.get.createChild(originalPath.toAbsolutePath.parent.hashCode.toString, asDirectory = true)

      // By prepending the modtime we prevent collisions in the cache from files that have changed in between.
      // Md5 is safer but much much slower and way too CPU intensive for big files.
      val pathAndModTime: String = originalPath.lastModifiedTime.toEpochMilli.toString + originalPath.name
      val cachedCopyPath: Path = cachedCopySubDir./(pathAndModTime)

      // The following step cannot be performed multithreaded. Checking if the file exists and copying of the file should never
      // happen simultaneously. Therefore this is synchronized using an object that only exists once in the JVM.
      // This is probably not the proper 'akka' way of doing things. But I could not find the proper way of doing things
      // after an extensive web search and read through the akka documentation.

      SharedFileSystem.synchronized {
        if (!cachedCopyPath.exists) {
          val cachedCopyTmpPath = cachedCopyPath.plusExt("tmp")
          originalPath.copyTo(cachedCopyTmpPath, overwrite = true).moveTo(cachedCopyPath)
        }
      }
      cachedCopyPath.linkTo(executionPath)
    }.void
    logOnFailure(action, "cached copy file")
  }

  implicit def actorContext: ActorContext

  lazy val DefaultStrategies = Seq("hard-link", "soft-link", "copy")

  lazy val LocalizationStrategyNames: Seq[String] = getConfigStrategies("localization")
  lazy val LocalizationStrategies: Seq[DuplicationStrategy] = createStrategies(LocalizationStrategyNames, docker = false)
  lazy val DockerLocalizationStrategies: Seq[DuplicationStrategy] = createStrategies(LocalizationStrategyNames, docker = true)

  lazy val CachingStrategies: Seq[String] = getConfigStrategies("caching.duplication-strategy")
  lazy val Cachers: Seq[DuplicationStrategy] = createStrategies(CachingStrategies, docker = false)

  private def getConfigStrategies(configPath: String): Seq[String] = {
    if (sharedFileSystemConfig.hasPath(configPath)) {
      sharedFileSystemConfig.getStringList(configPath).asScala
    } else {
      DefaultStrategies
    }
  }

  private def createStrategies(configStrategies: Seq[String], docker: Boolean): Seq[DuplicationStrategy] = {
    // If localizing for a docker job, remove soft-link as an option
    val filteredConfigStrategies = configStrategies filter {
      case "soft-link" if docker => false
      case "cached-copy" if cachedCopyDir.isEmpty => false
      case _ => true
    }

    // Convert the (remaining) config strategies to duplication strategies
    val mappedDuplicationStrategies = filteredConfigStrategies map {
      case "hard-link" => localizePathViaHardLink _
      case "soft-link" => localizePathViaSymbolicLink _
      case "copy" => localizePathViaCopy _
      case "cached-copy" => localizePathViaCachedCopy _
      case unsupported => throw new UnsupportedOperationException(s"Strategy $unsupported is not recognized")
    }

    // Prepend the default duplication strategy, and return the sequence
    localizePathAlreadyLocalized _ +: mappedDuplicationStrategies
  }

  def hostAbsoluteFilePath(jobPaths: JobPaths, pathString: String): Path = {
    val path = PathFactory.buildPath(pathString, pathBuilders)
    path match {
      case _: DefaultPath if !path.isAbsolute => jobPaths.callExecutionRoot.resolve(path).toAbsolutePath
      case _: DefaultPath if jobPaths.isInExecution(path.pathAsString) => jobPaths.hostPathFromContainerPath(path.pathAsString)
      case _: DefaultPath => jobPaths.hostPathFromContainerInputs(path.pathAsString)
    }
  }

  def outputMapper(job: JobPaths)(womValue: WomValue): Try[WomValue] = {
    WomFileMapper.mapWomFiles(mapJobWomFile(job), Set.empty)(womValue)
  }

  def mapJobWomFile(jobPaths: JobPaths)(womFile: WomFile): WomFile = {
    val hostPath = hostAbsoluteFilePath(jobPaths, womFile.valueString)
    def hostAbsolute(pathString: String): String = hostAbsoluteFilePath(jobPaths, pathString).pathAsString

    if (!hostPath.exists) throw new FileNotFoundException(s"Could not process output, file not found: ${hostAbsolute(womFile.valueString)}")

    // There are composite WomFile types like WomMaybeListedDirectoryType that need to make the paths of contained
    // WomFiles host absolute, so don't just pass in a `const` of the function call result above.
    womFile mapFile hostAbsolute
  }

  def cacheCopy(sourceFilePath: Path, destinationFilePath: Path): Try[Unit] = {
    duplicate("cache", sourceFilePath, destinationFilePath, Cachers.toStream, docker = false)
  }

  /**
   * Return a possibly altered copy of inputs reflecting any localization of input file paths that might have
   * been performed for this `Backend` implementation.
   */
  def localizeInputs(inputsRoot: Path, docker: Boolean)(inputs: WomEvaluatedCallInputs): Try[WomEvaluatedCallInputs] = {
    TryUtil.sequenceMap(
      inputs safeMapValues WomFileMapper.mapWomFiles(localizeWomFile(inputsRoot, docker), Set.empty),
      "Failures during localization"
    ) recoverWith {
      case e => Failure(new IOException(e.getMessage) with CromwellFatalExceptionMarker)
    }
  }

  def localizeWomFile(inputsRoot: Path, docker: Boolean)(value: WomFile): WomFile = {
    val strategies = if (docker) DockerLocalizationStrategies else LocalizationStrategies

    // Strip the protocol scheme
    def stripProtocolScheme(path: Path): Path = DefaultPathBuilder.get(path.pathWithoutScheme)

    /*
      * Transform an original input path to a path in the call directory.
      * The new path matches the original path, it only "moves" the root to be the call directory.
      */

    def toCallPath(womFile: WomFile)(path: String): Try[PairOfFiles] = Try {
      val src = buildPath(path)
      // Strip out potential prefix protocol
      val localInputPath = stripProtocolScheme(src)
      val dest = if (inputsRoot.isParentOf(localInputPath)) localInputPath
      else {
        val nameOverride = womFile match {
          case directory: WomMaybeListedDirectory => directory.basename
          case _ => None
        }
        // Concatenate call directory with absolute input path
        DefaultPathBuilder.get(inputsRoot.pathAsString, localInputPath.parent.pathAsString.hashCode.toString, nameOverride.getOrElse(localInputPath.name))
      }

      PairOfFiles(src, dest)
    }

    // A possibly staged version of the input file suitable for downstream processing, or just the original input
    // file if no staging was required.
    val staged: WomFile = value.mapFile { input =>
      pathBuilders.collectFirst({ case h: HttpPathBuilder if HttpPathBuilder.accepts(input) => h }) match {
        case Some(httpPathBuilder) =>
          implicit val materializer = ActorMaterializer()
          implicit val ec = actorContext.dispatcher

          Await.result(httpPathBuilder.content(input).map { _.toString }, Duration.Inf)
        case _ => input
      }
    }

    // Optional function to adjust the path to "docker path" if the call runs in docker
    localizeWomFile(toCallPath _, strategies.toStream, docker)(staged)
  }

  /**
    * Try to localize a WomFile.
    *
    * @param toDestPath function specifying how to generate the destination path from the source path
    * @param strategies strategies to use for localization
    * @param womFile WomFile to localize
    * @return localized WomFile
    */
  private def localizeWomFile(toDestPath: WomFile => String => Try[PairOfFiles], strategies: Stream[DuplicationStrategy], docker: Boolean)
                             (womFile: WomFile): WomFile = {
    val localized = womFile mapWomFile { file =>
      val result = toDestPath(file)(file.value) flatMap {
        case PairOfFiles(src, dst) => duplicate("localize", src, dst, strategies, docker).map(_ => dst.pathAsString)
      }
      result.get
    }
    val sized = localized collect {
      case womMaybePopulatedFile@WomMaybePopulatedFile(Some(path), _, None, _, _, _, _) =>
        val pair = toDestPath(womMaybePopulatedFile)(path).get
        val srcSize = pair.src.size
        womMaybePopulatedFile.copy(sizeOption = Option(srcSize))
    }
    sized
  }
}
