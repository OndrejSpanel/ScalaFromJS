package com.github.opengrabeso

import org.scalatest.FunSuite

class RuleTests extends FunSuite with TestUtils {
  test("Delete member variables and functions") {
    execute check ConversionCheck(
      //language=JavaScript
      """
      function C() {

      }

      C.prototype.constructor = C;

      C.prototype.naturalFunction = function() {
          this.naturalMember = 0;
          this.exoticMember = 0;
      };

      C.prototype.exoticFunction = function() {};

      var ScalaFromJS_settings = {
          members: [
              {
                  cls: ".*",
                  name: "exotic.*",
                  operation: "delete"
              },
          ]
      };
      """)
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

  test("Handle method extending") {
    execute check ConversionCheck(
      // language=JavaScript
      """
      class Person {
          constructor(name) {
              this.name = name;
          }

          equals(that) {
              return this.name = that.name;
          }

      }

      let bob = new Person('Bob');
      let dan = new Person('Dan');
      let same = bob.equals(dan);

      var ScalaFromJS_settings = {
          members: [
              {
                  cls: ".*",
                  name: "equals",
                  operation: "subst",
                  template: [
                      "$this",
                      "override def $name(that: Any) = {",
                      "  obj match {",
                      "    case v: $class => this.equals(v)",
                      "    case _ => false",
                      "  }",
                      "}",
                    ]
              }]
      };
      """).required(
        "def equals(that: Person)",
        "override def equals(that: Any)"
      ).forbidden(
        "override def equals(that: Person)"
      )


  }

}
