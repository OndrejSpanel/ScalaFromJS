package com.github.opengrabeso

import org.scalatest.FunSuite

class ArrayTests extends FunSuite with TestUtils {
  test("Map and Array types should be inferred") {
    execute check ConversionCheck(
      //language=JavaScript
      """
      function f(x,y,w) {
          var z = {};
          var index = 0;
          var name = "name";
          var fa = [1, 2, 3];
          x[index] = "x";
          y[name] = x;
          z[name] = "z";
          if (!w) w = ["A", "B", "C"];
      }

      function C() {
          this.ca = [0, 1, 2];
          this.cb = [];
      }

      C.prototype.constructor = C;

      C.prototype.fc = function() {
          this.cc = [];
      }
      """)
      .required(
        "x: Array[String]",
        "y: Map[String, Array[String]]",
        "x(index)",
        "y(name)",
        "z = Map.empty[String, String]",
        "var cc = Array.empty[Unit]",
        "var ca = Array(0, 1, 2)",
        "var cb = Array.empty"
      ).forbidden(
      "[index]","[name]"
    )
  }

  test("Array types should be inferred from item operations") {
    execute check ConversionCheck(
      //language=JavaScript
      """
      function C() {
          this.ca = [0, 1, 2];
          this.cb = [];
          this.cd = [];
          this.ce = [];
      }

      C.prototype.constructor = C;

      C.prototype.fc = function() {
          this.cb.push("A");
          this.cd = ["X", "Y", "Z"];
          this.ce[0] = 123;
          this.x0 = this.ca[0];
      }
      """)
      .required(
        "var x0: Double",
        "var cb = Array.empty[String]",
        "var cd = Array.empty[String]",
        "var ce = Array.empty[Double]"
      )
  }

}
