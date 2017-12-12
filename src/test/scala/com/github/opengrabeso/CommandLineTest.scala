package com.github.opengrabeso

import org.scalatest.FunSuite

class CommandLineTest extends FunSuite with TestUtils {

  import CommandLine._

  def forEachFileWithCleanup(files: Seq[String])(f: String => Unit): Unit = {
    try {
      files.foreach(f)
    } finally {
      try {
        files.foreach(removeFile)
      } catch {
        // ignore errors while removing files, to avoid them hiding the test failure
        case scala.util.control.NonFatal(_)  =>
      }
    }
  }

  def convertProject(controlFile: String): String = {
    withTempDir("ScalaFromJS-test-") { temp =>
      val out = convertFileToFile("src/test/resources/" + controlFile, temp + "xxx.scala")
      val sb = new StringBuilder
      forEachFileWithCleanup(out) { f =>
        // for each file verify the resulting file is a Scala file with a comment
        assert(f.endsWith(".scala"))
        val outCode = readFile(f)
        sb append outCode
        exec check ResultCheck(outCode).required("/*", "*/")
      }
      sb.result
    }
  }

  test("Single file conversion") {
    withTempDir("ScalaFromJS-test-") { temp =>
      val out = convertFileToFile("src/test/resources/files/a.js", temp + "aaa.scala")
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
        "import something."
      ).forbidden(
        "package a.js",
        "package src",
        "package test"
      )
  }
}
