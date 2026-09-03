package com.github.opengrabeso.scalafromjs

import org.scalatest.funsuite.AnyFunSuite

class FunctionTests extends AnyFunSuite with TestUtils {

  test("Handle function conversion") {
    exec check ConversionCheck("" +
      """
         function f() {}
         function a(x) {}
         function b(x, y) {return x}
      """
    ).required(
      "def f()",
      "def a(x: Any)",
      "def b(x: Any, y: Any)",
    )
  }

  test("Handle arrow function conversion") {
    exec check ConversionCheck("" +
      """
         const f = () => {}
         const a = (x) => {}
         const b = (x, y) => {return x}
         const c = (x, y) => x
      """
    ).required(
      "def f()",
      "def a(x: Any)",
      "def b(x: Any, y: Any)",
      "def c(x: Any, y: Any)",
    )

  }
  test("Handle object destructuring in function parameters of arrow functions") {
    exec check ConversionCheck("" +
      """
       const f = ({a, b}) => {}
      """
    )
  }

  test("Handle aliased object destructuring defaults in function parameters") {
    exec check ConversionCheck(
      "const f = ({ normalView: normalViewAlias = normalView }) => normalViewAlias"
    ).required("def f(normalViewAlias: Any = normalView)")
  }

  test("Handle a default value for an object destructuring parameter") {
    exec check ConversionCheck(
      "class TileCreasedNormalsPlugin { constructor({ creaseAngle = Math.PI / 3 } = {}) {} }"
    ).required("creaseAngle: Double = Math.PI / 3")
  }

  test("Handle array destructuring in function parameters of arrow functions") {
    exec check ConversionCheck("" +
      """
       const f = ([a, b]) => {}
      """
    )
  }
}
