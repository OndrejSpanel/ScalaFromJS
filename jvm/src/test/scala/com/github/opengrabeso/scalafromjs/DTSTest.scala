package com.github.opengrabeso.scalafromjs

import org.scalatest.funsuite.AnyFunSuite

class DTSTest extends AnyFunSuite with TestUtils with ProjectUtils {
  test("Types from d.ts should override any inference") {
    val outCode = convertProject("d.ts/input.js")
    exec check ResultCheck(outCode)
      .required(
        "def createCSI(): CSI",
        "trait CSI",
        "extends CSI",
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

  test("d.ts derived class complex constructor") {
    val outCode = convertProject("complex-constructor.d.ts/input.js")
    exec check ResultCheck(outCode)
      .required(
        "class Base(",
        "class Derived", "extends Base("
      )
  }

  test("d.ts resolve type aliases") {
    val outCode = convertProject("type-alias.d.ts/input.js")
    exec check ResultCheck(outCode)
      .required(
        "def getAB(): String",
        "var dab: String",
        "var dabEx: String"
      ).forbidden(
      ": AB"
    )
  }

}
