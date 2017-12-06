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

  test("Handle getClass") {
    execute check ConversionCheck(
      //language=JavaScript
      """
      class Node {
        type () {return "Node"}
      }

      class Identifier extends Node {
        type () {return Syntax.Identifier}
      }

      class Literal extends Node {
        type () {return Syntax.Literal}
      }

      Syntax = {
        Identifier : 'Identifier',
        Literal: 'Literal'
      };

      function useIfSimple(a) {
        if (a.type === Syntax.Identifier) {
          return a.name
        }
        return ""
      }

      function useExpr(b) {
        return b.type === Syntax.Identifier && b.name === value
      }

      function useIf(c) {
        if (c.type === Syntax.Identifier && c.name.lenght > 0) {
          return c.name
        }
        return ""
      }

      function useExprComplex(d) {
        return (
          d.type === Syntax.Identifier && d.name === value ||
          d.type === Syntax.Literal && d.value === value
        );
      }
      var ScalaFromJS_settings = {
        members: [
          {
            cls: ".*",
            name: "type",
            operation: "getClass"
          }
        ]
      }
      """).required(
        "case a_cast: Identifier",
        "b.isInstanceOf[Identifier] && b.asInstanceOf[Identifier].name",
        "c.isInstanceOf[Identifier] && c.asInstanceOf[Identifier].name",
        "d.isInstanceOf[Identifier] && d.asInstanceOf[Identifier].name",
        "d.isInstanceOf[Literal] && d.asInstanceOf[Literal].value"
      ).forbidden(
        "Syntax."
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

  test("Handle symbol scope removal") {
    execute check ConversionCheck(
      // language=JavaScript
      """

      let bob = new Some.Scope.Person('Bob');
      let dan = new Other.Scope.Person('Dan');
      let pete = new Scope.Person('Pete');

      Scope.Middle.Person.func();

      var ScalaFromJS_settings = {
          symbols: [
              {
                  name: "Some/Scope",
                  operation: "remove"
              },
              {
                  name: "Scope",
                  operation: "remove"
              },
              ]
      };
      """).required(
      """new Person("Bob")""",
      """new Other.Scope.Person("Dan")""",
      """new Person("Pete")"""
    ).forbidden(
      "new Some.Scope.",
      "new Scope."
    )


  }
}
