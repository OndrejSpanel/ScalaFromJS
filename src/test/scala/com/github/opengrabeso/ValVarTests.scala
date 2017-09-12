package com.github.opengrabeso

import org.scalatest.FunSuite

class ValVarTests extends FunSuite with TestUtils {
  test("Val detection") {
    execute check ConversionCheck(rsc("expressions/variables.js"))
      .required(
        "val s =",
        "var x =",
        "var y",
        "var z",
        "val u =",
        "val a =",
        "val l ="
      ).forbidden(
      "var a",
      "var l",
      "val x",
      "val y",
      "var u"
    )
  }

  test("Detect object instead of variable") {
    execute check ConversionCheck(
      // language=JavaScript
      """
      var g = {
        hi: "Hi"
      }
      """).required(
        "object g"
      ).forbidden(
        "val g",
        "var g"
      )

  }


}
