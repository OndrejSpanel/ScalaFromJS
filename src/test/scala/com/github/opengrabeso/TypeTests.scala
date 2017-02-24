package com.github.opengrabeso

import org.scalatest.FunSuite
import Resources.{getResource => rsc}

class TypeTests extends FunSuite with TestUtils {
  test("Detect vals") {
    // note: overlaps expression/variables (ExpressionTests / "Val detection")
    execute check ConversionCheck(rsc("types/inference.js"))
      .required(
        "val i = 1",
        "val d = 1.2",
        "val i = 1",
        "var di"
      )
      .forbidden("function")
  }

  test("Infer var type") {
    execute check ConversionCheck(rsc("types/inference.js"))
      .required(
        "var ii = 1",
        "var di = 1.2",
        //"var aa : Any = 1", // not yet
        "first: String",
        "last: String",
        "x: String"
      )
  }

  test("Function parameters and calls (with JSDoc)") {
    execute check ConversionCheck(rsc("types/functionsJSDoc.js")).
      required(
        "full = first + last",
        """result = concatenate("Zara", "Ali")""",
        "first: String",
        "last: String",
        "def concatenate(",
        "def secondFunction()",
        "full"
      ).forbidden(
      "function", "Any", "return full"
    )
  }

  test("Infer parameter and return types") {
    execute check ConversionCheck(rsc("types/inference.js"))
      .required(
        ""
      )

  }

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
        "class Janitor extends Employee",
        "def printEmployeeDetails() ="
    )

  }
}