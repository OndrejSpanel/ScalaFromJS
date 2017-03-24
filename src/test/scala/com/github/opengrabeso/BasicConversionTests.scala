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
      "function",
      "return"
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
    val result = Convert(rsc("control/indent.js"))
    assert(result == normalizeEol(rsc("control/indent.scala")))
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
    ) // return not present. How to test?
  }

}
