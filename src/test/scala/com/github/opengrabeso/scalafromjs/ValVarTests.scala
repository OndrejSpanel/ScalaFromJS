package com.github.opengrabeso.scalafromjs

import org.scalatest.FunSuite

class ValVarTests extends FunSuite with TestUtils {
  test("Val detection") {
    exec check ConversionCheck(rsc("expressions/variables.js"))
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

  test("Double var merge") {
    exec check ConversionCheck(
      // language=JavaScript
      """
      function f() {
        var a = 0;
        var a = 1;
      }
      function g() {
        if (true) {
          var c = 1;
        } else {
          var c = 2;
        }
      }
      for (var i = 0; i < 10; i++ ) {
      }

      for (var i = 0; i < 10; i++ ) {
      }
      """).required(
      "var a = 0",
      "val c = 1",
      "val c = 2",
      "a = 1",
      "for (i <- 0 until 10)"
    ).forbidden(
      "val a",
      "var a = 1",
      "while"
    )

  }

  test("Var scope should be detected properly ") {
    exec check ConversionCheck(
      // language=JavaScript
      """
      function f() {
        var x = 0
      }

      var x = ""
      """).required(
      "val x = 0",
      """val x = """""
    ).forbidden(
      "val a",
      "var a = 1",
      "while"
    )

  }

  test("Detect object instead of variable") {
    exec check ConversionCheck(
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
    exec check ConversionCheck(
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
    exec check ConversionCheck(
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
    exec check ConversionCheck(
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

  test("Handle default values for function parameters") {
    exec check ConversionCheck(
      // language=JavaScript
      """
      function f( x, y, z, w ) {
        var fx = x || 0;
        var fy = y || 0;
        var fz = ( z === undefined) ?  0 : z;
        var fw = ( w !== undefined ) ? w : 1;
      }
      """).required(
        "def f(x: Double = 0, y: Double = 0, z: Double = 0, w: Double = 1)",
        "val fx = x",
        "val fy = y",
        "val fz = z",
        "val fw = w"
      ).forbidden(
        "val gl"
      )

  }

  test("Handle default values for class constructor parameters") {
    exec check ConversionCheck(
      // language=JavaScript
      """
      function V4( x, y, z, w ) {

       this.x = x || 0;
       this.y = y || 0;
       this.z = z || 0;
       this.w = ( w !== undefined ) ? w : 1;

      }

      new V4
      """).required(
        "class V4(var x: Double = 0, var y: Double = 0, var z: Double = 0, var w: Double = 1)"
      ).forbidden(
        "_par"
      )
  }
}
