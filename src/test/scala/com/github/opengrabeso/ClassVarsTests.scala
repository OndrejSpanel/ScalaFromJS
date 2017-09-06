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
}
