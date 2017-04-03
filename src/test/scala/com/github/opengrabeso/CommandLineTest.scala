package com.github.opengrabeso

import org.scalatest.FunSuite

class CommandLineTest extends FunSuite with TestUtils {

  import CommandLine._

  test("testResolveSibling") {
    assert(resolveSibling("temp/a.js", "x.scala") == "temp/x.scala")
  }

  test("testChangeExtension") {
    assert(changeExtension("temp/a.js", "out/b.scala") == "temp/a.scala")
    assert(changeExtension("temp/a.js", "out.xxx/b.scala") == "temp/a.scala")
    assert(changeExtension("temp.xx/a.js", "out.xxx/b") == "temp.xx/a.")
  }

  test("testShortName") {
    assert(shortName("temp/a.js") == "a.js")
    assert(shortName("a.js") == "a.js")
  }


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
  test("Single file conversion") {
    withTempDir("ScalaFromJS-test-") { temp =>
      val out = convertFileToFile("src/test/resources/files/a.js", temp + "aaa.scala")
      assert(out.nonEmpty)
      forEachFileWithCleanup(out) { f =>
        val outCode = readFile(f)
        execute check ResultCheck(outCode).required("def A()")
      }
    }
  }

  test("Multiple file conversion") {
    withTempDir("ScalaFromJS-test-") { temp =>
      val out = convertFileToFile("src/test/resources/files/input.js", temp + "xxx.scala")
      forEachFileWithCleanup(out) { f =>
        assert(f.endsWith(".scala"))
        val outCode = readFile(f)
        execute check ResultCheck(outCode).required("/*", "*/", "def ", "() =")
      }
    }
  }
}
