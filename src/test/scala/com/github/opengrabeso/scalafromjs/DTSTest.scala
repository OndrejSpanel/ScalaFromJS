package com.github.opengrabeso.scalafromjs

import org.scalatest.FunSuite

class DTSTest extends FunSuite with TestUtils with ProjectUtils {
  test("Types from d.ts should override any inference") {
    val outCode = convertProject("d.ts/input.js")
    exec check ResultCheck(outCode)
      .required(
        "r: String", "m: Double", "bnum: Double", "bstr: String", "xnum: Double",
        "var num: Double", "var str: String",
        "def bMember(bpa: String, bpb: Double)",
        "class C(var cn: Double, var cs: String, var cb: Boolean)",
        "class CX(var cn: Double, var cs: String, var cb: Boolean)"
      )
  }
}
