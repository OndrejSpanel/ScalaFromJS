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
    )
  }

  test("Function type members") {
    exec check ConversionCheckTypeScript(
      """
          interface D<T> {};
          class C {
            nFunString: (a: string, b: string) => string;
            nFunComplex: (n: number, d: D<number>) => D<string>;
          }
      """
    ).required(
      "trait D[T]",
      "nFunString: (String, String) => String",
      // TODO: "nFunComplex: (Double, D[Double) => D[String]",
    )
  }

  test("TypeScript interface and implements conversion") {
    exec check ConversionCheckTypeScript(
      """
            interface C {
              num: number;
              str: string;
            }
            class D implements C {
              constructor(dnp: number, dsp: string){
                this.num = dnp;
                this.str = dsp;
              };
            }
        """
    ).required(
      "trait C",
      "extends C"
    ).forbidden("_par")
  }

  test("Static class members") {
    exec check ConversionCheckTypeScript(
      """
            class D {
              static fun(dnp: number, dsp: string): string {
                return dsp;
              };
            }
        """
    ).required(
      "object D",
      "def fun(dnp: Double, dsp: String): String"
    ).forbidden("Any")
  }

  test("Namespace as object") {
    exec check ConversionCheckTypeScript(
      """
            export namespace D {
              export function fun(dnp: number, dsp: string): string {
                return dsp;
              };
            }
        """
    ).required(
      "object D",
      "def fun(dnp: Double, dsp: String): String"
    ).forbidden("Any")
  }

  test("Support generic classes") {
    pendingUntilFixed {
      exec check ConversionCheckTypeScript(
        //language=TypeScript
        """
            class C<T> {
              member: T;
              anyMember: any;
              fun(): T {return this.anyMember;};
            }

            var cs = new C<string>();
            var cn = new C<number>();
            var fs;
            var fn;
            var ms;
            var mn;
            if (true) {
              fs = cs.fun();
              fn = cn.fun();
              ms = cs.member;
              mn = cn.member;
            }

        """
      ).required(
        "class C[T]",
        "new C[String]",
        "new C[Double]",
        "var fs: String",
        "var fn: Double",
        "var ms: String",
        "var mn: Double",
      ).forbidden("Any")
    }
  }
}