package com.github.opengrabeso.esprima

import com.github.opengrabeso.TestUtils

trait TestInputs extends TestUtils {
  val answer42 ="answer = 42"

  //language=JavaScript
  val es6 = """
      class Node {
        type () {return "Node"}
      }

      class Identifier extends Node {
        type () {return Syntax.Identifier}
        letter(i){return i;}
      }

      class Literal extends Node {
        type () {return Syntax.Literal}
      }

      Syntax = {
        Identifier : 'Identifier',
        Literal: 'Literal'
      };

      function useIfSimple(key) {
        if (key.type === Syntax.Identifier) {
          return key.name
        }
        return ""
      }

      function useExpr(key) {
        return key.type === Syntax.Identifier && key.name === value
      }

      function useIf(key) {
        if (key.type === Syntax.Identifier && key.name.lenght > 0) {
          return key.name
        }
        return ""
      }

      function useExprComplex(key, value) {
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
      """

  lazy val threeSource = rsc("three.js")

  lazy val esprimaSource = rsc("esprima.js")
}
