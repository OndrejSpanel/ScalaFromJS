package com.github.opengrabeso.scalafromjs

import org.scalatest.funsuite.AnyFunSuite

class ArrayTests extends AnyFunSuite with TestUtils {
  test("Map and Array types should be inferred") {
    exec check ConversionCheck(
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
    exec check ConversionCheck(
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
    exec check ConversionCheck(
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
    exec check ConversionCheck(
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
    exec check ConversionCheck(
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

  test("new array.constructor should be handled") {
    exec check ConversionCheck(
      //language=JavaScript
      """
      var a, b, c;

      if (true) {
        a= new array.constructor( array.subarray( from, to !== undefined ? to : array.length ) );
        b = new source.array.constructor( source.array );
        c = new array.constructor(indices.length * itemSize);
      }
      """)
      .required(
        "var a = Array.empty",
        "var b = Array.empty",
        "var c = Array.empty"
      )

  }

  test("Various forms of array constructions should be handled") {
    exec check ConversionCheck(
      //language=JavaScript
      """

        var fa = new Float32Array(10);

        var aa = new Array(10);

        var pa = [0, 1, 2];

        var vf, va, vp;

        if (true) {
          vf = fa;
          va = aa;
          vp = pa;
        }
      """)
      .required(
        "var vf = Array.empty",
        "var va = Array.empty",
        "var vp = Array.empty"
      )

  }

  test("Array access with mixed types should be handled fine") {

    exec check ConversionCheck(
      //language=JavaScript
      """
        var f = new Float32Array(10);

        var a = new Array(10);

        var p = [0, 1, 2];

        var vf, va, vp;

        var ef, ea, ep;

        var bb;

        if (true) {
          vf = f;
          va = a;
          vp = p;

          ef = f[0];
          ea = a[0];
          ep = p[0];

          bb = new Array(10);
          bb = new Float32Array(10);

          bb[0];

          bb[0] = bb[0]
        }
      """)
      .required(
        "var ep: Double",
      ).forbidden(
        "Array[Array"
      )
  }

  test("Avoid recursive array nesting when mixing array and non-array access") {
    exec check ConversionCheck(
      //language=JavaScript
      """
        class G {
          constructor( s ) {
            s = Array.isArray( s ) ? s : [ s ];
          }
        }

        class S {}

        var s = new S()

        var e1 = new G(s)

        var e2 = new G([s, s])

        var e3, e4, e5;

        e3 = e4;

        e3 = e5;

        e5 = e1;
      """)
      .required(
        "Array[S]"
      ).forbidden("Array[Array[")

  }


}
