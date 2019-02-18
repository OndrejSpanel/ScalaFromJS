package com.github.opengrabeso.scalafromjs

import org.scalatest.FunSuite
class PostprocessTests extends FunSuite with TestUtils {
  test("Delete member variables and functions") {
    exec check ConversionCheck(
      //language=JavaScript
      """

    let bob = new Some.Scope.Person('Bob');
    let dan = new Other.Scope.Person('Dan');
    let pete = new Scope.Person('Pete');

    var ScalaFromJS_settings = {
        symbols: [
            {
                name: "Some/Scope",
                operation: "remove"
            }
        ],
        postprocess: [
            {
                operation: "replace",
                pattern: "Bob",
                replace: "Bill"
            },
            {
                operation: "replace",
                pattern: "val (.*) = new",
                replace: "object $1 extends"
            }
        ]
    };
    """
    ).required(
      "object bob extends Person(\"Bill\")",
      "object dan extends Other.Scope.Person(\"Dan\")",
      "object pete extends Person(\"Pete\")"
    ).forbidden(
      "Bob"
    )
  }


  test("Handle console.warn removal") {
    exec check ConversionCheck(
      //language=JavaScript
      """

      function keepWarn() {
        console.warn("Some warning")
      }

      function noLongerDefault(x) {
        if (x === undefined) {
            console.warn("Parameter x is now required");
            x = 0.0
        }
        return x;
      }

      var ScalaFromJS_settings = {
          preprocess: [
              {
                  operation: "replace",
                  pattern: "console\\.warn\\(\".*is now required.*\"\\)",
                  replace: ""
              }
          ]
      };
      """)
      .required(
        "def noLongerDefault",
        "Some warning",
        "def keepWarn",
        "def noLongerDefault(x: Double = 0.0)"
      ).forbidden(
      "is now required"
    )
  }


}
