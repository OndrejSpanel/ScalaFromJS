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

  test("Smart Array.isArray conditional handling") {
    execute check ConversionCheck(
      //language=JavaScript
      """
      function f( shapes ) {
       shapes = Array.isArray( shapes ) ? shapes : [ shapes ];
      }

      var x1 = a1;
      var a1 = x2;
      var x2 = "";
      """)
      .required(
        "shapes_par: Array[Unit]"
      ).forbidden(
        "Array[Array["
      )
  }

  test("Map and Array types should be inferred from property access") {
    execute check ConversionCheck(
      //language=JavaScript
      """
      function C() {
          this.mb = {};
      }

      C.prototype.constructor = C;

      C.prototype.getMap1 = function (id) {
        return this.mb[id]
      };

      C.prototype.getMap2 = function () {
        return this.ma[""]
      };

      C.prototype.getArray = function () {
        return this.aa[0]
      }
      """)
      .required(
        "var ma = Map.empty[String,",
        "var aa = Array.empty[",
        "var mb = Map.empty[String,"
      )
  }

  test("Array types should be inferred for multiple items, including function calls") {
    execute check ConversionCheck(
      //language=JavaScript
      """
      var array;
      var a1;
      if (true) {
        a1 = f1();
        array = [f1(), f1()]
      }

      function f1(){return 0;}
      """)
      .required(
        "var array = Array.empty[Double]",
        "var a1: Double"
      ).forbidden(
        "array = Array.empty[Any]"
      )
  }
}
