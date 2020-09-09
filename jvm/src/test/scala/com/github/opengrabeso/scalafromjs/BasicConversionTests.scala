package com.github.opengrabeso.scalafromjs

import org.scalatest.funsuite.AnyFunSuite

class BasicConversionTests extends AnyFunSuite with TestUtils {


  test("Simple functions") {
    exec check ConversionCheck(rsc("simpleFunction/simpleFunctions.js"))
      .required(
        "def firstFunction()",
        "def secondFunction()"
      ).forbidden(
      "function",
      "return"
    )
  }

  test("Unsupported code") {
    exec check ConversionCheck(rsc("unsupported/unsupported.js"))
      .required("/* Unsupported: Break */ break")
      .forbiddenNothing
  }

  test("String escaping") {
    exec check ConversionCheck("""var string = "Multiple lines\nAnd some tabs\tas well\r\n"""")
      .required("\\n", "\\r", "\\t")
      .forbidden("\t")
  }

  test("Indenting") {
    val result = normalizeEol(Convert(rsc("control/indent.js"), header = false))
    val expected = normalizeEol(rsc("control/indent.scala"))
    assert(result == expected)
  }

  test("Reserved words") {
    exec check ConversionCheck(
      """function def()
        |{
        |    var val;
        |    var match;
        |    return object
        |}""".stripMargin
    ).required("`def`", "`val`", "`match`", "`object`")
  }

  test("Sequences") {
    exec check ConversionCheck(
      """function f() {
        |  return 0, 1, 2;
        |}""".stripMargin
    ) // return not present. How to test?
  }

}
