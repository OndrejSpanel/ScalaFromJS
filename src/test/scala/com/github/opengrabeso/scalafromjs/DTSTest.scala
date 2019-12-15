package com.github.opengrabeso.scalafromjs

import org.scalatest.FunSuite

class DTSTest extends FunSuite with TestUtils with ProjectUtils {
  test("Types from d.ts should override any inference") {
    pendingUntilFixed {
      val outCode = convertProject("d.ts/input.js")
      exec check ResultCheck(outCode)
        .required("x: String")
        .forbidden("x: Double", "x: Any", "x: Unit")
    }
  }
}
