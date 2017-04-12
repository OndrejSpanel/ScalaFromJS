package com.github.opengrabeso

import org.scalatest.FunSuite

class ClassVarsTests extends FunSuite with TestUtils {
  test("Handle class variables") {
    execute check ConversionCheck(
      // language=JavaScript
      """
      function f(a_par) {
        var a = a_par;
        a = 1;
      }


      function C(width, height) {
        this.w = width;
      }

      C.prototype.constructor = C;
      """).required(
        "(a_par:",
        "var w:"
      ).forbidden(
      "(var a",
      "width_par"
    )

  }
}
