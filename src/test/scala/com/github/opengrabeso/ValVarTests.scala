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
      function W() {
          var wg = {
              hi: "Hi"
          }
      }

      var g = {
          hi: "Hi"
      };

      var w = new W()
      """).required(
        "object g",
        "object wg"
      ).forbidden(
        "val wg","var wg",
        "val g","var g"
      )

  }

  test("Do not detect object when var is reassigned") {
    execute check ConversionCheck(
      // language=JavaScript
      """
      function W() {
          var wg = {
              hi: "Hi"
          };
          wg = undefined;
      }

      var g = {
          hi: "Hi"
      };
      g = undefined;

      var w = new W()
      """).required(
        "var g",
        "var wg"
      ).forbidden(
        "object g",
        "object wg"
      )

  }

  test("Detect object instead of variable even when used in a function") {
    execute check ConversionCheck(
      // language=JavaScript
      """
      function W() {
          var wg = {
              hi: "Hi"
          };
          var wf = function() {return wg}
      }

      var g = {
          hi: "Hi"
      };
      var f = function() {return g};

      var w = new W()
      """).required(
      "object g",
      "object wg"
    ).forbidden(
      "val wg","var wg",
      "val g","var g"
    )

  }

  test("Private variable which cannot be initialized on construction should not be a val") {
    execute check ConversionCheck(
      // language=JavaScript
      """
      function GL() {
        var x;
        var gl = getGL();

        this.destroy = function() {
            gl.fun();
        };

        return this;

      }

      var p = new GL();
      """).required(
        "var gl",
        "this.gl ="
      ).forbidden(
        "val gl"
      )

  }


}
