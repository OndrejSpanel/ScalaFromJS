package com.github.opengrabeso

import Resources.{getResource => rsc}
import org.scalatest.FunSuite

class TypeTests extends FunSuite with TestUtils {
  test("Detect vals, infer var type") {
    // note: overlaps expression/variables (ExpressionTests / "Val detection")
    execute check ConversionCheck(rsc("types/inference.js"))
      .required(
        "val i = 1",
        "val d = 1.2",
        "val i = 1",
        "var ii = 1",
        "var di = 1.2",
        //"var aa : Any = 1", // not yet
        "first: String",
        "last: String",
        "x: String"
      )
      .forbidden("function")
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

  test("Correctly infer subtypes / supertypes in various positions") {
    execute check ConversionCheck(rsc("types/subtypes.js"))
      .required(
        "def fx(px: X)",
        "def fa(pa: A)",
        "def fb(pb: B)",
        "var xx: X",
        "var yy: X",
        "var xa: A",
        "var pp: A"
      )
  }

  test("Correctly infer types from Math functions") {
    execute check ConversionCheck(rsc("types/math.js"))
      .required(
        "var x: Double",
        "var y: Double"
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

  test("Default values should be inferred for parameters") {
    execute check ConversionCheck(rsc("types/defaultValues.js"))
      .required(
        "a: Any",
        "d: Double = 0",
        "c: Double = 1",
        "b: Double = 2",
        "aa: Any",
        "dd: Double = 0",
        "cc: Double = 1",
        "bb: Double = 2"
      )
  }

  test("JS class in IIFE") {
    execute check ConversionCheck(rsc("types/classVariants.js"))
      .required(
        "class Background",
        "def init() =",
        "class BoxGeometry",
        "extends Geometry",
        "class HemisphereLight",
        "extends Light",
        "class Clock",
        "class Object3D"
      )
      .forbidden(
        ".prototype",
        "Background =", // assignment of the IIFE result should be removed
        "}()" // IIFE should be removed
      )
  }

  test("JS class with a constructor and super calls") {
    execute check ConversionCheck(rsc("types/classSuper.js"))
      .required(
        "class Animal",
        "class Snake",
        "extends Animal",
        "def move(meters: Double) ="
      )
      .forbidden(
        ".prototype",
        ".call"
      )
  }

  test("Classes should be inferred based on used members") {
    execute check ConversionCheck(rsc("types/classesByMembers.js"))
      .required(
        "cx: X",
        "cy: XY",
        "cxy: XY",
        "cxyz: XYZ",
        "t: Any"
      )
  }

  test("Class properties should be defined") {
    execute check ConversionCheck(rsc("types/classSuper.js"))
      .required(
        "def fullName =",
        "def fullName_=(value",
        "def isSnake =",
        "var kind = \"snake\""
      )
  }

  test("Static members should be handled as objects") {
    execute check ConversionCheck(rsc("types/static.js"))
      .required(
        "object Cls",
        "var defX = ",
        "def defY() = ",
        "var z = ",
        "object Utils",
        "var pi =",
        "def funA(",
        "def funB(",
        "var funC = (",
        "Cls.z = 1",
        "Utils.x = 11"
      ).forbidden(
        "Cls.defX =",
        "Cls.defY ="
      )
  }
}