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

  test("TypeScript enum conversion") {
    exec check ConversionCheckTypeScript(
      """
      export enum E {
        E0,
        E1,
        E100 = 100,
        E101
      }
      """
    ).required(
      "object E extends Enumeration",
      "val E0 = Value()",
      "val E1 = Value()",
      "val E100 = Value(100)",
      "val E101 = Value()",
    ).forbidden("_par", " elements = elements")
  }

  test("Object type members") {
    exec check ConversionCheckTypeScript(
      """
          interface D {};
          class C {
            nData: {[index: string]: number};
            dData: {[index: number]: D};
            oData: {
              on: number;
              os: string;
            };
          }
      """
    ).required(
      "nData = Map.empty[String, Double]",
      "dData = Map.empty[String, D]",
    ).forbidden("object D")
  }

}
