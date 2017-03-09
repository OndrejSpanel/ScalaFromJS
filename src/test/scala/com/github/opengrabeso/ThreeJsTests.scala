package com.github.opengrabeso

import Resources.{getResource => rsc}

import org.scalatest.FunSuite

class ThreeJsTests extends FunSuite with TestUtils {

  test("Convert Three.js Vector3 with reasonable results") {
    pendingUntilFixed {
      execute check ConversionCheck(rsc("threejs/vector3.js"))
        .required(
          "class Vector3(x: Double = 0, y: Double = 0, z: Double = 0)",
          "(axis: Vector3, angle: Double)",
          "def dot(v: Vector3)"
        )
        .forbidden(
          "return (axis, angle)",
          "break",
          "console.warn"
        )
    }
  }
}
