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


}
