package com.github.opengrabeso.scalafromjs

import org.scalatest.FunSuite

class BasicConversionTests extends FunSuite with TestUtils {


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
    exec check ConversionCheck(""""Multiple lines\nAnd some tabs\tas well\r\n"""")
      .required("\\n", "\\r", "\\t")
      .forbidden("\t")
  }

  test("Indenting") {
    val result = Convert(rsc("control/indent.js"), header = false)
    assert(result == normalizeEol(rsc("control/indent.scala")))
  }

  test("Reserved words") {
    exec check ConversionCheck(
      """function def()
        |{
        |    var val;
        |    var match;
        |    return yield
        |}""".stripMargin
    ).required("`def`", "`val`", "`match`", "`yield`")
  }

  test("Sequences") {
    exec check ConversionCheck(
      """function f() {
        |  return 0, 1, 2;
        |}""".stripMargin
    ) // return not present. How to test?
  }

}
