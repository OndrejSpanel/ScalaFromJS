package com.github.opengrabeso.scalafromjs

import org.scalatest.funsuite.AnyFunSuite

class MultipleGlobals extends AnyFunSuite with TestUtils {
  test("Make multiple global variables unique") { // detectDoubleVars
    exec check ConversionCheck(
      // language=JavaScript
      """
      var x = 0;
      function a( ap ) {
        return x == ap;
      }
      var x = "";
      function b( bp ) {
        x = bp;
      }
      var x = false;
      function c() {
        return x;
      }
      """).required(
      "x = 0",
      "x$1 = \"\"",
      "x$2 = false",
      "ap: Double",
      "bp: String"
    )
  }

  test("Renaming a global must not rename locals") { // detectDoubleVars
    exec check ConversionCheck(
      // language=JavaScript
      """
      var x = 0;
      function a( ap ) {
        return x === ap;
      }
      var x = "";
      function b( bp ) {
        function bLocal(){
          var x = bp;
          return x;
        }
        x = bp;
      }
      var x = false;
      class C {
        constructor(x) {
          this.x = x;
        }
        set x(v){this.x = v;}
      }
      class D {
        constructor() {
          this.x = 0;
        }
        fun() {
          this.x = this.xx;
        }
      }
      function c() {
        return x;
      }
      """).required(
      "def x_=(",
      "val x = bp"
    ).forbidden("def x$")
  }
}


