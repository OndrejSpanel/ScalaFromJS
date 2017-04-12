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
}
