package com.github.opengrabeso.scalafromjs

import org.scalatest.FunSuite

class CommandLineTest extends FunSuite with TestUtils with ProjectUtils {
  import CommandLine._

  test("Single file conversion") {
    withTempDir("ScalaFromJS-test-") { temp =>
      val out = convertFileToFile(rscPath("files/a.js"), temp + "aaa.scala")
      assert(out.nonEmpty)
      forEachFileWithCleanup(out) { f =>
        val outCode = readFile(f)
        exec check ResultCheck(outCode).required("def A()")
      }
    }
  }

  test("Multiple file conversion") {
    val outCode = convertProject("files/input.js")
    exec check ResultCheck(outCode)
      .required("/*", "*/", "def ", "() =", "def A", "def B", "def D")
      .forbidden("def C", "def E")
  }

  test("Multiple file conversion with non-js files") {
    val outCode = convertProject("nonJSFiles/input.js")
    exec check ResultCheck(outCode)
      .required(
        "This is a plain text file, to be packed verbatim, as data.",
        "var value =",
        "object plain extends Resource {"
      ).forbidden(
        "isResource"
      )
  }

  test("Multiple file conversion with rules") {
    val outCode = convertProject("folderRules/input.js")
    exec check ResultCheck(outCode)
      .required(
        "package my.name"
      ).forbidden(
        "package a.js",
        "package src",
        "package test"
      )
  }

  test("Multiple file conversion with wrong rules") {
    intercept[UnsupportedOperationException] {
      convertProject("folderRules/ruleError.js")
    }
  }

  test("Multiple file conversion with file wrapping") {
    val outCode = convertProject("folderRules/wrapFile.js")
    exec check ResultCheck(outCode)
      .required(
        "package my.name",
        "class SomeTest extends Tests",
        "object dd extends Data",
        "val value =",
        "import something."
      ).forbidden(
        "package a.js",
        "package src",
        "package test"
      )
  }
}
