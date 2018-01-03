package wom.values

import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.{FlatSpec, Matchers}
import wom.WomExpressionException
import wom.types._

import scala.util.{Success, Try}

class WomFileSpec extends FlatSpec with Matchers with TableDrivenPropertyChecks {

  behavior of "WomFile"

  lazy val singleDir = WomUnlistedDirectory("single/dir")
  lazy val singleFile = WomSingleFile("single/file")
  lazy val globFile = WomGlobFile("glob/*")
  lazy val listedDir1 = WomUnlistedDirectory("listed/dir1")
  lazy val listedDir2 = WomUnlistedDirectory("listed/dir2")
  lazy val secondaryFile1 = WomSingleFile("secondary/file1")
  lazy val secondaryFile2 = WomSingleFile("secondary/file2")
  lazy val dirWithListedDirs = WomListedDirectory(singleDir.value, List(listedDir1, listedDir2))
  lazy val dirWithListedFiles = WomListedDirectory(singleDir.value, List(secondaryFile1, secondaryFile2))
  lazy val fileWithSecondaryFiles = WomPrimaryFile(singleFile.value, List(secondaryFile1, secondaryFile2), None)
  lazy val fileWithSecondaryDirs = WomPrimaryFile(singleFile.value, List(listedDir1, listedDir2), None)
  lazy val nestedFilesAndDirs = WomPrimaryFile(
    value = singleFile.value,
    secondaryFiles = List(WomListedDirectory(
      value = listedDir1.value,
      listing = List(WomPrimaryFile(
        value = secondaryFile1.value,
        secondaryFiles = List(WomListedDirectory(listedDir2.value, List(secondaryFile2))),
        formatOption = None
      ))
    )),
    formatOption = None
  )

  val mapFileTests = Table(
    ("description", "womFile", "expected"),
    ("a single directory", singleDir, WomUnlistedDirectory("prepend/single/dir")),
    ("a single file", singleFile, WomSingleFile("prepend/single/file")),
    ("a glob file", globFile, WomGlobFile("prepend/glob/*")),
    ("a dir with listed dirs", dirWithListedDirs,
      WomListedDirectory(
        "prepend/single/dir",
        List(WomUnlistedDirectory("prepend/listed/dir1"), WomUnlistedDirectory("prepend/listed/dir2"))
      )
    ),
    ("a dir with listed files", dirWithListedFiles,
      WomListedDirectory(
        value = "prepend/single/dir",
        listing = List(WomSingleFile("prepend/secondary/file1"), WomSingleFile("prepend/secondary/file2"))
      )
    ),
    ("a file with secondary files", fileWithSecondaryFiles,
      WomPrimaryFile(
        value = "prepend/single/file",
        secondaryFiles = List(WomSingleFile("prepend/secondary/file1"), WomSingleFile("prepend/secondary/file2")),
        formatOption = None
      )
    ),
    ("a file with secondary dirs", fileWithSecondaryDirs,
      WomPrimaryFile(
        value = "prepend/single/file",
        secondaryFiles = List(WomUnlistedDirectory("prepend/listed/dir1"), WomUnlistedDirectory("prepend/listed/dir2")),
        formatOption = None
      )
    ),
    ("a nested file/dir", nestedFilesAndDirs,
      WomPrimaryFile(
        value = "prepend/single/file",
        secondaryFiles = List(WomListedDirectory(
          value = "prepend/listed/dir1",
          listing = List(WomPrimaryFile(
            value = "prepend/secondary/file1",
            secondaryFiles = List(WomListedDirectory(
              value = "prepend/listed/dir2",
              listing = List(WomSingleFile("prepend/secondary/file2"))
            )),
            formatOption = None
          ))
        )),
        formatOption = None
      )
    )
  )

  forAll(mapFileTests) { (description, womFile, expected) =>
    it should s"map $description" in {
      womFile.mapFile("prepend/" + _) should be(expected)
    }
  }

  it should "test valueString" in {
    singleDir.valueString should be("single/dir")
    singleFile.valueString should be("single/file")
    globFile.valueString should be("glob/*")
  }

  it should "test womString" in {
    singleDir.toWomString should be(""""single/dir"""")
    singleFile.toWomString should be(""""single/file"""")
    globFile.toWomString should be("""glob("glob/*")""")
  }

  val addTests = Table(
    ("description", "womFile", "expectedPrefix", "expectedSuffix"),
    ("a single directory", singleDir, WomString("prefix/single/dir"), WomUnlistedDirectory("single/dir/suffix")),
    ("a single file", singleFile, WomString("prefix/single/file"), WomSingleFile("single/file/suffix")),
    ("a glob file", globFile, WomString("prefix/glob/*"), WomGlobFile("glob/*/suffix"))
  )

  forAll(addTests) { (description, womFile, expectedPrefix, expectedSuffix) =>
    it should s"add a string prefix to $description" in {
      WomString("prefix/").add(womFile) should be(Success(expectedPrefix))
    }

    it should s"add a string suffix to $description" in {
      womFile.add(WomString("/suffix")) should be(Success(expectedSuffix))
    }

    it should s"add an optional string prefix to $description" in {
      WomOptionalValue(WomStringType, Option(WomString("prefix/"))).add(
        WomOptionalValue(womFile.womType, Option(womFile))
      ) should be(Success(expectedPrefix))
    }

    it should s"add an optional string suffix to $description" in {
      WomOptionalValue(womFile.womType, Option(womFile)).add(
        WomOptionalValue(WomStringType, Option(WomString("/suffix")))
      ) should be(Success(expectedSuffix))
    }

  }

  val addFailures = Table(
    ("description", "womFile", "expected"),
    ("a dir with listed dirs", dirWithListedDirs,
      "WomListedDirectory(" +
        "single/dir,List(WomUnlistedDirectory(listed/dir1), WomUnlistedDirectory(listed/dir2))" +
        ")"),
    ("a dir with listed files", dirWithListedFiles,
      "WomListedDirectory(" +
        "single/dir,List(WomSingleFile(secondary/file1), WomSingleFile(secondary/file2))" +
        ")"),
    ("a file with secondary files", fileWithSecondaryFiles,
      "WomPrimaryFile(" +
        "single/file,List(WomSingleFile(secondary/file1), WomSingleFile(secondary/file2)),None" +
        ")"),
    ("a file with secondary dirs", fileWithSecondaryDirs,
      "WomPrimaryFile(" +
        "single/file,List(WomUnlistedDirectory(listed/dir1), WomUnlistedDirectory(listed/dir2)),None" +
        ")"),
    ("a nested file/dir", nestedFilesAndDirs,
      "WomPrimaryFile(" +
        "single/file,List(WomListedDirectory(" +
        "listed/dir1,List(WomPrimaryFile(" +
        "secondary/file1,List(WomListedDirectory(" +
        "listed/dir2,List(WomSingleFile(secondary/file2))" +
        ")),None" +
        "))" +
        ")),None" +
        ")")
  )

  forAll(addFailures) { (description, womFile, expected) =>
    it should s"fail to add a string prefix to $description" in {
      the[WomExpressionException] thrownBy {
        WomString("prefix/").add(womFile).get
      } should have message s"Cannot perform operation: prefix/ + $expected"
    }

    it should s"fail to add a string suffix to $description" in {
      the[WomExpressionException] thrownBy {
        womFile.add(WomString("/suffix")).get
      } should have message
        s"Cannot perform operation: $expected + WomString(/suffix)"
    }

    it should s"fail to add an optional string prefix to $description" in {
      the[WomExpressionException] thrownBy {
        WomOptionalValue(WomStringType, Option(WomString("prefix/"))).add(
          WomOptionalValue(womFile.womType, Option(womFile))
        ).get
      } should have message s"Cannot perform operation: prefix/ + $expected"
    }

    it should s"fail to add an optional string suffix to $description" in {
      the[WomExpressionException] thrownBy {
        WomOptionalValue(womFile.womType, Option(womFile)).add(
          WomOptionalValue(WomStringType, Option(WomString("/suffix")))
        ).get
      } should have message
        s"Cannot perform operation: $expected + WomOptionalValue(WomStringType,Some(WomString(/suffix)))"
    }
  }

  val addInvalidIntegerTests = Table(
    ("description", "womFile", "expected"),
    ("a single directory", singleDir, "Cannot perform operation: single/dir + WomInteger(42)"),
    ("a single file", singleFile, "Cannot perform operation: single/file + WomInteger(42)"),
    ("a glob file", globFile, "Cannot perform operation: glob/* + WomInteger(42)")
  )

  forAll(addInvalidIntegerTests) { (description, womFile, expected) =>
    it should s"fail to add an int to $description" in {
      womFile.add(WomInteger(42)).failed.get should have message expected
    }

    it should s"fail to add an optional int to $description" in {
      WomOptionalValue(womFile.womType, Option(womFile)).add(
        WomOptionalValue(WomIntegerType, Option(WomInteger(42)))
      ).failed.get should have message expected
    }
  }

  val womFileEqualsTests = Table(
    ("description", "womFileA", "womFileB", "expected"),
    ("a single directory matched to a similar directory", singleDir, WomUnlistedDirectory(singleDir.value), true),
    ("a single file matched to a similar file", singleFile, WomSingleFile(singleFile.value), true),
    ("a glob file matched to a similar glob", globFile, WomGlobFile(globFile.value), true),
    ("a single directory matched to a dissimilar directory", singleDir, WomUnlistedDirectory("should/not/match"),
      false),
    ("a single file matched to a dissimilar file", singleFile, WomSingleFile("should/not/match"), false),
    ("a glob file matched to a dissimilar glob", globFile, WomGlobFile("should/not/match"), false)
  )

  forAll(womFileEqualsTests) { (description, womFileA, womFileB, expected) =>
    it should s"expect $expected comparing $description" in {
      womFileA.equals(womFileB) should be(Success(WomBoolean(expected)))
    }

    it should s"expect $expected symmetrically comparing $description" in {
      womFileB.equals(womFileA) should be(Success(WomBoolean(expected)))
    }

    it should s"expect $expected comparing (optionally) $description" in {
      WomOptionalValue(womFileA.womType, Option(womFileA)).equals(
        WomOptionalValue(womFileB.womType, Option(womFileB))
      ) should be(Success(WomBoolean(expected)))
    }

    it should s"expect $expected symmetrically comparing (optionally) $description" in {
      WomOptionalValue(womFileB.womType, Option(womFileB)).equals(
        WomOptionalValue(womFileA.womType, Option(womFileA))
      ) should be(Success(WomBoolean(expected)))
    }
  }

  val womFileEqualsFailures = Table(
    ("description", "womFileA", "womFileB"),
    ("a single file matched to a similar directory", singleFile, WomUnlistedDirectory(singleDir.value)),
    ("a glob file matched to a similar directory", globFile, WomUnlistedDirectory(singleDir.value)),
    ("a single directory matched to a similar file", singleDir, WomSingleFile(singleFile.value)),
    ("a glob file matched to a similar file", globFile, WomSingleFile(singleFile.value)),
    ("a single directory matched to a similar glob", singleDir, WomGlobFile(globFile.value)),
    ("a single file matched to a similar glob", singleFile, WomGlobFile(globFile.value)),
    ("a dir with listed dirs matched to a similar directory", dirWithListedDirs, WomUnlistedDirectory(singleDir.value)),
    ("a dir with listed files matched to a similar directory", dirWithListedFiles,
      WomUnlistedDirectory(singleDir.value)),
    ("a file with secondary files matched to a similar file", fileWithSecondaryFiles, WomSingleFile(singleFile.value)),
    ("a file with secondary dirs matched to a similar file", fileWithSecondaryDirs, WomSingleFile(singleFile.value)),
    ("a nested file/dir matched to a similar file", nestedFilesAndDirs, WomSingleFile(singleFile.value))
  )

  forAll(womFileEqualsFailures) { (description, womFileA, womFileB) =>
    it should s"fail comparing $description" in {
      a[WomExpressionException] should be thrownBy {
        womFileA.equals(womFileB).get
      }
    }

    it should s"fail symmetrically comparing $description" in {
      a[WomExpressionException] should be thrownBy {
        womFileB.equals(womFileA).get
      }
    }

    it should s"fail comparing (optionally) $description" in {
      a[WomExpressionException] should be thrownBy {
        WomOptionalValue(womFileA.womType, Option(womFileA)).equals(
          WomOptionalValue(womFileB.womType, Option(womFileB))
        ).get
      }
    }

    it should s"fail symmetrically comparing (optionally) $description" in {
      a[WomExpressionException] should be thrownBy {
        WomOptionalValue(womFileB.womType, Option(womFileB)).equals(
          WomOptionalValue(womFileA.womType, Option(womFileA))
        ).get
      }
    }
  }

  val womStringEqualsTests = Table(
    ("description", "womFile", "string", "expected"),
    ("a single directory matched to a similar directory", singleDir, singleDir.value, true),
    ("a single file matched to a similar directory", singleFile, singleDir.value, false),
    ("a glob file matched to a similar directory", globFile, singleDir.value, false),
    ("a single directory matched to a similar file", singleDir, singleFile.value, false),
    ("a single file matched to a similar file", singleFile, singleFile.value, true),
    ("a glob file matched to a similar file", globFile, singleFile.value, false),
    ("a single directory matched to a similar glob", singleDir, globFile.value, false),
    ("a single file matched to a similar glob", singleFile, globFile.value, false),
    ("a glob file matched to a similar glob", globFile, globFile.value, true)
  )

  forAll(womStringEqualsTests) { (description, womFile, string, expected) =>
    it should s"expect $expected comparing $description as a string" in {
      womFile.equals(WomString(string)) should be(Success(WomBoolean(expected)))
    }

    it should s"expect $expected symmetrically comparing $description as a string" in {
      WomString(string).equals(womFile) should be(Success(WomBoolean(expected)))
    }

    it should s"expect $expected comparing $description as an optional string" in {
      WomOptionalValue(womFile.womType, Option(womFile)).equals(
        WomOptionalValue(WomStringType, Option(WomString(string)))
      ) should be(Success(WomBoolean(expected)))
    }

    it should s"expect $expected symmetrically comparing $description as an optional string" in {
      WomOptionalValue(WomStringType, Option(WomString(string))).equals(
        WomOptionalValue(womFile.womType, Option(womFile))
      ) should be(Success(WomBoolean(expected)))
    }
  }

  val womStringEqualsFailures = Table(
    ("description", "womFile", "string", "expected"),
    ("a dir with listed dirs matched to a similar directory", dirWithListedDirs, singleDir.value, true),
    ("a dir with listed files matched to a similar directory", dirWithListedFiles, singleDir.value, true),
    ("a file with secondary files matched to a similar file", fileWithSecondaryFiles, singleFile.value, true),
    ("a file with secondary dirs matched to a similar file", fileWithSecondaryDirs, singleFile.value, true),
    ("a nested file/dir matched to a similar file", nestedFilesAndDirs, singleFile.value, true),
    ("a dir with listed dirs matched to a similar directory", dirWithListedDirs, "should/not/match", false),
    ("a dir with listed files matched to a similar directory", dirWithListedFiles, "should/not/match", false),
    ("a file with secondary files matched to a similar file", fileWithSecondaryFiles, "should/not/match", false),
    ("a file with secondary dirs matched to a similar file", fileWithSecondaryDirs, "should/not/match", false),
    ("a nested file/dir matched to a similar file", nestedFilesAndDirs, "should/not/match", false)
  )

  forAll(womStringEqualsFailures) { (description, womFile, string, expected) =>
    it should s"expect $expected comparing $description as a string" in {
      a[WomExpressionException] should be thrownBy {
        womFile.equals(WomString(string)).get
      }
    }

    it should s"expect $expected symmetrically comparing $description as a string" in {
      a[WomExpressionException] should be thrownBy {
        WomString(string).equals(womFile).get
      }
    }

    it should s"expect $expected comparing $description as an optional string" in {
      a[WomExpressionException] should be thrownBy {
        WomOptionalValue(womFile.womType, Option(womFile)).equals(
          WomOptionalValue(WomStringType, Option(WomString(string)))
        ).get
      }
    }

    it should s"expect $expected symmetrically comparing $description as an optional string" in {
      a[WomExpressionException] should be thrownBy {
        WomOptionalValue(WomStringType, Option(WomString(string))).equals(
          WomOptionalValue(womFile.womType, Option(womFile))
        ).get
      }
    }
  }

  it should "produce an invalid equals" in {
    val result: Try[WomBoolean] = singleFile.equals(WomInteger(42))
    result.failed.get should have message "Cannot perform operation: single/file == WomInteger(42)"
  }
}
