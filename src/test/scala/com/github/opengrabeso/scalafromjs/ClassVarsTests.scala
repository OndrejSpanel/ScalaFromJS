package com.github.opengrabeso.scalafromjs

import org.scalatest.FunSuite

class ClassVarsTests extends FunSuite with TestUtils {
  test("Handle class variables") {
    exec check ConversionCheck(
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

  test("Infer type of class members when using class variables in prototype based classes") {
    exec check ConversionCheck(
      // language=JavaScript
      """
      function Person(name){
          this.name = name;
      }

      Person.prototype.set = function(n){
          this.name = n
      };

      var bob = new Person('Bob');
      """).required(
        "class Person(var name: String)",
        "def set(n: String)"
      ).forbidden(
        "Any"
      )

  }

  test("Infer type of class members when using class variables in ES6 classes") {
    exec check ConversionCheck(
      // language=JavaScript
      """
      class Person {
          constructor(name) {
              this.name = name;
          }

          set(n) {
              this.name = n;
          }
      }

      var bob = new Person("Bob")
      """).required(
        "class Person(var name: String)",
        "def set(n: String)"
      ).forbidden(
        "Any"
      )

  }

  test("Duplicate assignment prevents introducing a parameter variable") {
    exec check ConversionCheck(
      // language=JavaScript
      """
      class Person {
          constructor(name) {
              this.b = name;
              this.c = name
          }

      }

      let bob = new Person('Bob');
      """).required(
        "class Person(name: String)",
        "var b: String = name",
        "var c: String = name"
      ).forbidden(
        "(var name:"
      )

  }



  test("Member initializations after var inlined only when constant") {
    exec check ConversionCheck(
      // language=JavaScript
      """
      function Cls() {
        var x = 0;
        this.prop = true;
        this.props = "";
        this.a = [];
        this.b = [0];
        this.c = [something];
        this.d = Infinity;
        this.e = -1;
        this.f = 1 + 2 * 5;
      }

      var w = new Cls()
      """).required(
        "this.c =",
        "var a",
        "var b",
        "var c",
        "var prop",
        "var props"
      ).forbidden(
        "this.prop =",
        "this.props =",
        "this.a =",
        "this.b =",
        "this.d =",
        "this.e =",
        "this.f ="
      )

  }

  test("Private variables should be extracted") {
    exec check ConversionCheck(
      // language=JavaScript
      """
      function WebGLRenderer() {
        var _gl = {};
        var x = 0;
        this.getContext = function () {
          return _gl;
        };
      }
      var w = new WebGLRenderer()
      """).required(
      "val x =",
      "this._gl"
    ).forbidden(
      "this.x"
    )

  }

  test("Local variables from private functions should be left intact") {
    exec check ConversionCheck(
      // language=JavaScript
      """
      function WebGLRenderer() {
        var _gl = {};
        var x = 0;
        this.getContext = function () {
          var va;
          var vb = 0;
          var vc;
          vc = 0;
          return _gl;
        };
      }
      var w = new WebGLRenderer()
      """).required(
      "val x =",
      "var va =",
      "val vb =",
      "val vc =",
      "this._gl"
    ).forbidden(
      "this.x",
      "this.va",
      "this.vb",
      "this.vc"
    )

  }

  test("Local variables should be left intact when constructor is returning a prototype") {
    exec check ConversionCheck(
      // language=JavaScript
      """
      function F() {
        return {
          get: function () {
            var xx;
            return xx;
          }
        };
      }

      var f = new F();
      """).required(
        "var xx"
      ).forbidden(
        "this.xx"
      )

  }



  test("Private variables should not be created for known functions") {
    exec check ConversionCheck(
      // language=JavaScript
      """
      function C() {

        function f() {}

        function g() {
          f();
          this.a();
        }
      }

      C.prototype.constructor = C;
      """).required(
        "def f()",
        "def g()",
        "f()",
        "var a: () => "
      ).forbidden(
        "var f"
      )

  }


  test("Private variables should be extracted as object when appropriate") {
    exec check ConversionCheck(
      // language=JavaScript
      """
      function WebGLRenderer() {
        var _gl = {
          version: "0.1.2"
        };
        var x = {
          key: "Value"
        };
        this.getContext = function () {
          return _gl;
        };
        this.getX = function () {
          return x;
        };
        this.setX = function (xx) {
          x = xx;
        };
      }
      var w = new WebGLRenderer()
      """).required(
      "object _gl","var x"
    ).forbidden(
      "val _gl","var _gl","object x"
    )

  }

  test("Private variables should be extracted when initializer is constant") {
    exec check ConversionCheck(
      // language=JavaScript
      """
      function WebGLRenderer() {
        var sentinel;
        var a = 1;
        var b = 2;
        var c = a + b + fun();
        var d = a + b;
        var e = 1 + 2;
        this.f = function () {
          return a + b + c + d + e;
        };
      }
      var w = new WebGLRenderer()
      """).required(
      "val a",
      "val b",
      "var c",
      "val d",
      "val e"
    ).forbidden(
      "val c"
    )

  }

  test("Private variables should be extracted correctly for class parameters") {
    exec check ConversionCheck(
      // language=JavaScript
      """
      function C( x ) {

        var a, b;
        x = x || false;

        this.f = function f() {
          a = b;
          return x;
        }
      }

      C.prototype.constructor = C;
      """).required(
        "class C(var x: Boolean = false)",
        "var a",
        "var b",
        "this.a",
        "this.b",
        "this.x"
      ).forbidden(
        "val x"
      )

  }

  test("Constructor-local functions which are not exported (and their variables) should not be extracted as members") {
    exec check ConversionCheck(
      // language=JavaScript
      """
      function C() {
        var a, b, c, d;

        this.getA = function () {return a;};

        function getB() {return b;}
        function getCC() {return c;}
        function getD() {return d;}
        function getC() {return getCC();}
        this.getC = function(){return getC();};

        this.getD = getD;
      }
      var v = new C()
      """).required(
        "def getA", "var a", "this.a",
        "def getC", "def getCC", "var c", "this.c",
        "def getD", "var d", "this.d"
      ).forbidden(
        "this.b",
        "var getA",
        "var getB",
        "var getC",
        "var getD"
      )

  }

  test("Private functions in constructors returning prototypes should be extracted as members") {
    exec check ConversionCheck(
      // language=JavaScript
      """
      function C() {
        var x = false;

        return {
          setX: function ( xx ) {
            x = xx;
          },
          value: function () {
            var x = "";
            var a = x;
            return a;
          },
        };
      }

      new C
      """).required(
        "var x",
        "this.x = xx",
        "val x",
        "val a = x"
      ).forbidden(
        "this.a",
        "this.xx"
      )

  }




}
