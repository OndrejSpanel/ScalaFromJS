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

  test("Single file conversion") {
    val temp = tempDir("ScalaFromJS-test-")
    val out = convertFileToFile("src/test/resources/files/a.js", temp + "aaa.scala")
    out.foreach { f =>
      val outCode = readFile(f)
      execute check ResultCheck(outCode).required("def A()")
    }

  }
}
