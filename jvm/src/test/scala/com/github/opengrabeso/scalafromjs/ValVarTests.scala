package com.github.opengrabeso.scalafromjs

import org.scalatest.funsuite.AnyFunSuite

class ValVarTests extends AnyFunSuite with TestUtils {
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

  test("Global double var resolution") {
    exec check ConversionCheck(
      // language=JavaScript
      """
      var a = 0;
      function f() {
        return a;
      }
      var a = "";
      function g() {
        return a;
      }
      var a = null;
      function h() {
        a = new Object();
        return a;
      }
      var r1, r2, r3;
      if (true) {
        r1 = f();
        r2 = g();
        r3 = h();
      }
      """).required(
      "val a = 0",
      "val a$1 = \"\"",
      "var a$2 = null",
      "var r1: Double",
      "var r2: String",
      "var r3: Object"
    ).forbidden()

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

  test("Detect nested object") {
    pendingUntilFixed {
      exec check ConversionCheck(
        // language=JavaScript
        """
      var g = {
          hi: "Hi",
          prop: {
              there: 0
          },
      };
      """).required(
        "object g",
        "object prop"
      ).forbidden(
        "val g", "var g",
        "val prop", "var prop"
      )
    }
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
      function a(v = 0.0) {}
      """).required(
        "def f(x: Double = 0, y: Double = 0, z: Double = 0, w: Double = 1)",
        "val fx = x",
        "val fy = y",
        "val fz = z",
        "val fw = w",
        "def a(v: Double = 0.0)"
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

  test("Handle destructuring assignment") {
    exec check ConversionCheck(
      //lang=JavaScript
      """class Context {};

        var context = "";

        class Pars {
          get context() {return new Context;}
        };

        var x, y;
        if (true) {
          x = new Pars().context
          y = new R(new Pars()).c
        }

        class R {
          constructor(parameters) {

            const {
              context = null,
              options
            } = parameters;

            this.c = context;
          }
        }
        """
    ).required(
      "var x: Context",
      "var y: Context",
      "var c: Context"
    )
  }
}
