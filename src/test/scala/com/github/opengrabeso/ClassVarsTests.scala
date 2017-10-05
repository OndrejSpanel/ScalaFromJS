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

  test("Infer type of class members when using class variables in prototype based classes") {
    execute check ConversionCheck(
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
    execute check ConversionCheck(
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
    execute check ConversionCheck(
      // language=JavaScript
      """
      class Person {
          constructor(name) {
              this.name = name;
              this.c = name
          }

      }

      let bob = new Person('Bob');
      """).required(
        "class Person(name_par: String)",
        "var name: String = name_par",
        "var c: String = name_par"
      ).forbidden(
        "(var name:"
      )

  }



  test("Member initializations after var inlined only when constant") {
    execute check ConversionCheck(
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
    execute check ConversionCheck(
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

  test("Private variables should not be created for known functions") {
    execute check ConversionCheck(
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
    execute check ConversionCheck(
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
    execute check ConversionCheck(
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


}
