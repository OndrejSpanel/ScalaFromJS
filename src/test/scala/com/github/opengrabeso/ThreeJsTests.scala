package com.github.opengrabeso

import org.scalatest.FunSuite

class ThreeJsTests extends FunSuite with TestUtils {

  test("Convert Three.js Vector3 with reasonable results") {
    execute check ConversionCheck(rsc("threejs/vector3.js"))
      .required(
        "class Vector3(var x: Double = 0, var y: Double = 0, var z: Double = 0)",
        "def multiply(v: Vector3) =",
        "def multiplyScalar(scalar: Double) ="
      )
      .forbidden(
        "return (axis, angle)",
        "break",
        "console.warn"
      )
  }

  test("Convert Three.js math with usage examples with good results") {
    execute check ProjectCheck(rscPath("threejs/math-example.js")).required(
      "def dot(v: Vector3) =", // should be inferred from usage example
      "def applyAxisAngle(axis: Vector3, angle: Double) =" // should be inferred from Quaternion
    )

  }
}
