package com.github.opengrabeso

import org.scalatest.FunSuite
import Resources.{getResource => rsc}

class BasicConversionTests extends FunSuite with TestUtils {


  test("Simple functions") {
    execute check ConversionCheck(rsc("simpleFunction/simpleFunctions.js"))
      .required(
        "def firstFunction()",
        "def secondFunction()"
      ).forbidden(
      "function"
    )
  }

  test("Function parameters and calls") {
    execute check ConversionCheck(rsc("simpleFunction/callFunction.js")).
      required(
        "full = first + last",
        """result = concatenate("Zara", "Ali")""",
        "first: String",
        "last: String",
        "def concatenate(",
        "def secondFunction()"
      ).forbidden(
      "function", "Any"
    )
  }

  test("Flow control") {
    execute check ConversionCheck(rsc("control/control.js"))
      .required(
        "if (b) {",
        "a += 1",
        "if (!b)",
        "else {",
        "for (i <- 0 until 3)"
      ).forbidden("if (x) if (y)")
  }

  test("For loop special form") {
    execute check ConversionCheck(rsc("control/for.js"))
      .required(
        "for (a <- 0 until 10)",
        "while (s < 10)",
        "while (d < 10)",
        "var c = 0",
        "var d = 0",
        "c += 1",
        "d += 1",
        "s += 1"
      ).forbidden(
      "for (b <-",
      "for (c <-",
      "for (d <-"
    )
  }

  test("Unsupported code") {
    execute check ConversionCheck(rsc("unsupported/unsupported.js"))
      .required("/* Unsupported: Break */ break")
      .forbiddenNothing
  }

  test("String escaping") {
    execute check ConversionCheck(""""Multiple lines\nAnd some tabs\tas well\r\n"""")
      .required("\\n", "\\r", "\\t")
      .forbidden("\t")
  }

  test("Indenting") {
    val result = Main.convert(rsc("control/indent.js"))
    assert(result == normalizeEol(rsc("control/indent.scala")))
  }

  test("Simple class") {
    pending
    execute check ConversionCheck(rsc("simpleClass/simpleClass.js"))
      .required(
        "class Person",
        """var person = new Person("Bob", "M")"""
      )
      .forbidden(".prototype.")
  }

  test("Reserved words") {
    execute check ConversionCheck(
      """function def()
        |{
        |    var val;
        |    var match;
        |    return yield
        |}""".stripMargin
    ).required("`def`", "`val`", "`match`", "`yield`")
  }

  test("Sequences") {
    execute check ConversionCheck(
      """function f() {
        |  return 0, 1, 2;
        |}""".stripMargin
    ).required("return {")
  }

}
