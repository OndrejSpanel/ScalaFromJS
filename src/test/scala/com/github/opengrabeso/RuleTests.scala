package com.github.opengrabeso

import org.scalatest.FunSuite

class RuleTests extends FunSuite with TestUtils {
  test("Delete member variables and functions") {
    execute check ConversionCheck(rsc("rules/deleteMembers.js"))
      .required(
        "def natural",
        "var natural"
      ).forbidden(
        "def exotic",
        "var exotic"
     )
  }

  test("Replace member variables with properties") {
    execute check ConversionCheck(
      //language=JavaScript
      """
      function C(width, height) {
        this.parameters = {
          width: width,
          height: height
        };
        this.name = "C";
      }

      C.prototype.constructor = C;


      var ScalaFromJS_settings = {
          members: [
              {
                  cls: ".*",
                  name: "parameters",
                  operation: "make-property"
              },{
                  cls: ".*",
                  name: "name",
                  operation: "make-property"
              }]
      };
      """).required(
        "def parameters()",
        "def name()"
      )
  }

  test("Handle isClass") {
    execute check ConversionCheck(
      //language=JavaScript
      """
      function C(width, height) {
        this.parameters = {
          width: width,
          height: height
        };
        this.name = "C";
      }

      C.prototype.constructor = C;

      C.prototype.isC = true;
      C.prototype.isD = true;

      function f() {
          var c = new C();
          var a;
          if (c.isC && !a.isC) {}
          if (c.isA || c.isD) {}
      }

      var ScalaFromJS_settings = {
          members: [
              {
                  cls: ".*",
                  name: "is(.*)",
                  operation: "instanceof"
              }]
      };
      """).required(
        "c.isInstanceOf[C]",
        "c.isInstanceOf[C]",
        "c.isA",
        "c.isD"
      ).forbidden(
        "isInstanceOf[A]",
        "isInstanceOf[D]"
      )

  }
}
