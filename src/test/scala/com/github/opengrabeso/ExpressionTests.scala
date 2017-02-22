package com.github.opengrabeso

import Resources.{getResource => rsc}

import org.scalatest.FunSuite

class ExpressionTests extends FunSuite with TestUtils {

  test("Val detection") {
    execute check ConversionCheck(rsc("expressions/variables.js"))
      .required(
        "val s =",
        "var x =",
        "var y",
        "var z",
        "val a =",
        "val l ="
      ).forbidden(
      "var a",
      "var l",
      "val x",
      "val y",
      "var y =",
      "var z ="
    )
  }

  test("Handle unary operators") {
    execute check ConversionCheck(rsc("expressions/unary.js"))
      .required(
        "a += 1",
        "b += 1",
        """  a = {
          |    b
          |    b += 1
          |  }""".stripMargin,
        """  a = {
          |    c += 1
          |    c
          |  }""".stripMargin

      ).forbidden("--", "++")
  }

  test("Handle typeof / instanceof") {
    execute check ConversionCheck(rsc("expressions/types.js"))
      .required("s.isInstanceOf[String]", "s.getClass")
      .forbidden("instanceof", "typeof")
  }
}
