package com.github.opengrabeso

import org.scalatest.FunSuite

class ClassTests extends FunSuite with TestUtils {

  test("Simple JS 1.8 (ES 5) class") {
    execute check ConversionCheck(rsc("types/simpleClass.js"))
      .required(
        "class Person",
        """person = new Person("Bob", "M")"""
      )
      .forbidden(".prototype.")
  }

  test("Harmony (ES 6) class with inheritance") {
    execute check ConversionCheck(rsc("types/harmonyClass.js"))
      .required(
        "class Employe",
        "class Janitor",
        "extends Person",
        "extends Employee",
        "def nameGetter =",
        "def nameFunc() =",
        "val localVar = age",
        "def constructor(name: String, age: Double, salary: Double)",
        "class Person(var name: String, var age: Double)",
        "def printEmployeeDetails() ="
      )

  }

  test("Support local classes") {
    pendingUntilFixed {
      execute check ConversionCheck(
        // language=JavaScript
        """
        function f() {
          function C() {
              this.a = 0;
          }
          C.prototype.constructor = C;
          var c = new C();
          return c.a;
        }

        function g() {
          function C() {
              this.x = "X";
          }
          C.prototype.constructor = C;
          var c = new C();
          return c.x;
        }
        var v1, v2;
        if (true) v1 = f();
        if (true) v2 = g();
        """).required(
          "class C",
          "var v1: Double",
          "var v2: String"
        )
    }
  }

  test("Support local objects") {
    execute check ConversionCheck(
      // language=JavaScript
      """
      function f() {
        var pars = {
            a: 0,
            b: 1,
            c: 2
        };
        return pars.a;
      }

      function g() {
        var pars = {
            x: "X",
            y: "Y",
            z: "Z"
        };
        return pars.x;
      }
      var v1, v2;
      if (true) v1 = f();
      if (true) v2 = g();
      """).required(
      "object pars",
      "var a = ",
      "var x = "
    )
  }

}