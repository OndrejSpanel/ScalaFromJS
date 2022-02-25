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

  test("Handle array destructuring in function parameters of arrow functions") {
    exec check ConversionCheck("" +
      """
       const f = ([a, b]) => {}
      """
    )
  }
}