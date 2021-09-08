package com.github.opengrabeso.scalafromjs

import org.scalatest.funsuite.AnyFunSuite

class ObjectLiteralTests extends AnyFunSuite with TestUtils {
  test("Object literals should respect scope based hints - new") {
    exec check ConversionCheck(
      //language=JavaScript
      """
      class F {
        constructor() {
          this.a = {x: "X", y: "Y"};
          this.b = {x: "X", y: "Y"};
        }
      }

      var ScalaFromJS_settings = {

          hints: [
              {
                  path: ".*//.*a",
                  literals: "new"
              },
              {
                  path: ".*//.*b",
                  literals: "new X"
              }
          ]
      };
      """)
      .required("var a = new", "var b = new X")
  }

  test("Object literals should respect scope based hints - Map") {
    exec check ConversionCheck(
      //language=JavaScript
      """
      class F {
        constructor() {
          this.a = {x: "X", y: "Y"};
          this.b = {x: "X", y: "Y"};
        }
      }

      var ScalaFromJS_settings = {

          hints: [
              {
                  path: ".*//.*a",
                  literals: "Map"
              },
              {
                  path: ".*//.*b",
                  literals: "List"
              }
          ]
      };
      """)
      .required("var a = Map", "var b = List")
  }
}
