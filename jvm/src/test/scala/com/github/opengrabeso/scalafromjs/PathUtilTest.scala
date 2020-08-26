package com.github.opengrabeso.scalafromjs

import org.scalatest.funsuite.AnyFunSuite

class PathUtilTest extends AnyFunSuite with TestUtils {
  import PathUtils._

  test("testResolveSibling") {
    assert(resolveSibling("temp/a.js", "x.scala") == "temp/x.scala")

    assert(resolveSibling("temp/in/a.js", "../x.scala") == "temp/x.scala")

    assert(resolveSibling("temp/in/on/a.js", "../../x.scala") == "temp/x.scala")
    assert(resolveSibling("temp/in/on/a.js", "./../../x.scala") == "temp/x.scala")
    assert(resolveSibling("temp/in/on/a.js", "./.././../x.scala") == "temp/x.scala")

    assert(resolveSibling("temp/in/on/at/a.js", "../../../x.scala") == "temp/x.scala")
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

  test("relativePath") {
    assert(relativePath("temp/in/a.js", "temp/in/b.js") == "b.js")
    assert(relativePath("temp/in/a.js", "temp/in/on/b.js") == "on/b.js")
    assert(relativePath("temp/in/a.js", "temp/b.js") == "temp/b.js")
  }

}
