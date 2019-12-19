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
            class C { // TS style class - members declared
              num: number;
              str: string;
              constructor(cnp: number, csp: string){};
              cf(): number;
              cs(s: string): string;
            }
            class D { // mixed style JS / TS class - members assigned
              constructor(dnp: number, dsp: string){
                this.num = dnp;
                this.str = dsp;
              };
            }
        """
    ).required(
      "class C(cnp: Double", "csp: String)",
      "class D(var num: Double, var str: String)",
      "def cs(s: String)"
    ).forbidden("_par")
  }

  test("TypeScript class conversion - esprima ArrayExpression") {
    exec check ConversionCheckTypeScript(
      """
      export class ArrayExpression {
          readonly type: string;
          readonly elements: ArrayExpressionElement[];
          constructor(elements: ArrayExpressionElement[]) {
              this.type = Syntax.ArrayExpression;
              this.elements = elements;
          }
      }
      """
    ).required(
      "class ArrayExpression"
    ).forbidden("_par", " elements = elements")
  }

}
