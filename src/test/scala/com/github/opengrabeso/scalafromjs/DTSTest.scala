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
        "class CX(cn: Double, cs: String, cb: Boolean)",
        "def f(a: Double): Boolean =",
        "def g(a: String): Boolean =",
        "def f(a: Boolean, b: Boolean): Boolean =", // static functions - should be in object
        "def g(a: Boolean, b: Boolean): Boolean =",
        "dn: Double",
        "ds: String",
        "def createCSI(): CSI",
        "def fff(a: Double): Double",
        "def f(a: Double): Double",
        "var fv: Double",
        "var fs: String",
        "def gs: Boolean =",
        "def gs_=(b: Boolean): Unit =",
        "trait CSI",
        "extends CSI",
        "object SNamespace",
        "def sGenS(): String",
        "def sCompute(x: Double): Double"
      ).forbidden(
      "class SObj","trait SObj"
    )
  }

}
