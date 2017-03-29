package com.github.opengrabeso

import org.scalatest.FunSuite

class CommandLineTest extends FunSuite {

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

}
