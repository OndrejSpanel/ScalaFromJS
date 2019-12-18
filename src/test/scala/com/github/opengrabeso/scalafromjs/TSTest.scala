package com.github.opengrabeso.scalafromjs

import org.scalatest.FunSuite

class TSTest extends FunSuite with TestUtils with ProjectUtils {
  test("TypeScript variable conversion") {
    exec check ConversionCheckTypeScript(
      """
            var x: number;
            var s: string;
        """
    ).required("x: Double", "s: String")
  }

  test("TypeScript function conversion") {
    exec check ConversionCheckTypeScript(
      """
            function f(p: number, s: string): string {
              return s;
            }
        """
    ).required("def f(p: Double", "s: String)")
  }

  test("TypeScript class conversion") {
    exec check ConversionCheckTypeScript(
      """
            class C {
              num: number;
              str: string;
              constructor(cnp: number, csp: string){};
              cf(): number;
              cs(s: string): string;
            }
        """
    ).required(
      "class C(cnp: Double, csp: String)",
      "var num: Double",
      "var str: String",
      "def cf(): Double",
      "def cs(s: String): String"
    )
  }
}
