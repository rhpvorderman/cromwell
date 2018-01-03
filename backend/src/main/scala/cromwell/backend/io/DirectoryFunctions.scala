package cromwell.backend.io

import wom.expression.IoFunctionSet

// TODO: This will likely use a (tar file, dir list file) for each dirPath.
trait DirectoryFunctions extends IoFunctionSet {
  override def listAllFilesUnderDirectory(dirPath: String): Seq[String] = {
    temporaryImplListLocalFilesOnly(dirPath)
  }

  private final def temporaryImplListLocalFilesOnly(dirPath: String): Seq[String] = {
    import better.files._
    def listFiles(file: File): Seq[File] = {
      if (file.isDirectory)
        file.list.toList.flatMap(listFiles)
      else
        List(file)
    }

    listFiles(File(dirPath)).map(_.pathAsString)
  }
}
