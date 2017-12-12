package com.github.opengrabeso

import org.scalatest.FunSuite

class ClassVariantsTests extends FunSuite with TestUtils {

  test("Define class using basic prototype ES5 form") {
    exec check ConversionCheck(
      //language=JavaScript
      """
      function C() {}

      C.prototype.constructor = C;
      C.prototype.f = function (){};

      """).required(
       "class C",
        "def f()"
      ).forbidden(
        "prototype"
      )
  }

  test("Define class using basic prototype ES5 form with constructor not first") {
    exec check ConversionCheck(
      //language=JavaScript
      """
      C.prototype.constructor = C;
      C.prototype.f = function (){};

      function C() {}
      """).required(
      "class C",
      "def f()"
    ).forbidden(
      "prototype"
    )
  }

  test("Define class using Object.assign ES5 form") {
    exec check ConversionCheck(
      //language=JavaScript
      """
      function C() {}

      Object.assign(C.prototype, {
        constructor: C,
        f: function (){}
      })
      """).required(
      "class C",
      "def f()"
    ).forbidden(
      "prototype"
    )
  }

  test("Define class using Object.assign ES5 form with inheritance") {
    exec check ConversionCheck(
      //language=JavaScript
      """
      function C() {}

      Object.assign(C.prototype, {
        constructor: C,
        f: function (){}
      });

      function D() {}

      Object.assign(D.prototype, C.prototype, {
        constructor: D,
        fd: function (){}
      });

      """).required(
      "class C",
      "class D() extends C",
      "def f()",
      "def fd()"
    ).forbidden(
      "prototype"
    )
  }

  test("Mix ES6 and prototype class definition") {
    exec check ConversionCheck(
      //language=JavaScript
      """
      class C {
        x (){}
      }

      Object.assign(C.prototype, {
        f: function (){}
      });

      C.prototype.g = function (){};
      """).required(
        "class C",
        "def x()",
        "def f()",
        "def g()"
      ).forbidden(
        "prototype"
      )
  }
}
