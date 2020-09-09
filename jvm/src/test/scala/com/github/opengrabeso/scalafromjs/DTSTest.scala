package com.github.opengrabeso.scalafromjs

import org.scalatest.funsuite.AnyFunSuite

class DTSTest extends AnyFunSuite with TestUtils with ProjectUtils {
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

  test("d.ts enum conversion") {
    val outCode = convertProject("enum.d.ts/input.js")
    exec check ResultCheck(outCode)
      .required(
        "object A",
        "A0 = Value(0)",
        "A1 = Value(1)",
        "A2 = Value(2)",
        "type A = A.Value",
        "val A0: A = A.A0",
        "val A1: A = A.A1",
        "val A2: A = A.A2",
        "object E",
        "val E0 = Value(0)",
        "val E1 = Value(1)",
        "val E2 = Value(2)",
        "type E = E.Value",
        "var eVar: E",
        "val F0 = 0",
        "val F1 = 1",
        "val F2 = 2",
      ).forbidden(
      "F0:", "F1:", "F2:",
      "E0 = 0", "E1 = 1", "E2 = 2",
      )
  }

  test("d.ts base class inference") {
    pendingUntilFixed {
      val outCode = convertProject("base.d.ts/input.js")
      exec check ResultCheck(outCode)
        .required(
          "var i1: I1",
          "var i2: I2",
        ).forbidden(
        "var i1: A", "var i1: B", "var i1: Any",
        "var i2: A", "var i2: B", "var i2: Any"
      )
    }
  }

  test("d.ts ArrayLike handling") {
    val outCode = convertProject("arraylike.d.ts/input.js")
    exec check ResultCheck(outCode)
      .required(
        "ArrayLike[Double]"
      ).forbidden(
      )
  }

  test("d.ts mismatched and partial prototype handling") {
    val outCode = convertProject("mismatch.d.ts/input.js")
    exec check ResultCheck(outCode)
      .required(
        "a: Double", "d: String",
        "x: Double", "z: Boolean",
        "p0: Double", "p2: Boolean",
      )
  }
}
