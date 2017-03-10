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
          |    var temp = b
          |    b += 1
          |    temp
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

  test("Handle Scala keywords in identifiers") {
    execute check ConversionCheck(rsc("expressions/reserved.js"))
      .required(
        "val `type` =",
        "val `object` =",
        "def `match`(`val`: Animal)",
        "if (`val` == this) this",
        "`object`.`match`(x).move()",
        "def `lazy`(`object`: Any, meters: Double)"
      )
      .forbidden(
        "`this`",
        "`move`",
        "`Animal`",
        "`Snake`"
      )

  }

  test("Handle Immediately-invoked function expression (IIFE)") {
    execute check ConversionCheck(rsc("expressions/iife.js"))
      .required(
      )
      .forbidden(
        "return"
      )

  }


}
