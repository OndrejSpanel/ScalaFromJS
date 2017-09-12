package com.github.opengrabeso

import org.scalatest.FunSuite

class ExpressionTests extends FunSuite with TestUtils {

  test("Handle unary operators") {
    execute check ConversionCheck(rsc("expressions/unary.js"))
      .required(
        "a += 1",
        "b += 1",
        """  a = {
          |    var temp = b
          |    b += 1
          |    temp
          |  }""".stripMargin,
        """  a = {
          |    c += 1
          |    c
          |  }""".stripMargin

      ).forbidden("--", "++")
  }

  test("Handle typeof / instanceof") {
    execute check ConversionCheck(rsc("expressions/types.js"))
      .required("s.isInstanceOf[String]", ".getClass")
      .forbidden("instanceof", "typeof")
  }

  test("Handle Scala keywords in identifiers") {
    execute check ConversionCheck(rsc("expressions/reserved.js"))
      .required(
        "var `type`",
        "val `object` =",
        "def `match`(`val`: Animal)",
        "if (`val` == this) this",
        "`object`.`match`(x).move()",
        "def `lazy`(`object`:"
      )
      .forbidden(
        "`this`",
        "`move`",
        "`Animal`",
        "`Snake`"
      )

  }

  test("Handle Immediately-invoked function expression (IIFE)") {
    execute check ConversionCheck(rsc("expressions/iife.js"))
      .required(
      )
      .forbidden(
        "return"
      )

  }

  test("Remove trailing return in various positions") {
    execute check ConversionCheck(rsc("expressions/return.js"))
      .required(
      )
      .forbidden(
        "return"
      )
  }

  test("Handle delete (including property)") {
    execute check ConversionCheck(rsc("expressions/delete.js"))
      .required(
        "a -= \"x\"",
        "delete b"
      ).forbidden(
        "return"
      )
  }

  test("Handle binary operator priorities") {
    execute check ConversionCheck(
      //language=JavaScript
      """
      function f() {
         var a = 0 + 1 * 2;
         var b = (3 + 4) * 5;
         var c = (10 + 11) * (12 + 13);
         var d = 21 + 22 + 23;
         var e = 31 / (32 / 33);
         var f = 41 + 42 - 43 - (44 - 45);
         var g = 51 - (52 + 53);
         var h = 61 - 62 * 63;
         var i = ((71 + 72) & (73 * 74)) >= (75 | 76);
         a = 81 + 82;
         a += 91 + 92;
         var j = 93 <= 94 && 95 < 96;
         a = 101 << 102;
         a = 201 & 202;
      }
      """).required(
        "(3 + 4)",
        "(10 + 11)",
        "(12 + 13)",
        "21 + 22 + 23",
        "31 / (32 / 33)",
        "41 + 42 - 43",
        "(44 - 45)",
        "(52 + 53)",
        "61 - 62 * 63",
        "((71 + 72) & (73 * 74)) >= (75 | 76)"
      ).forbidden(
        "(1 * 2)",
        "(81 + 82)",
        "(91 + 92)",
        "(93 <= 94)",
        "(95 < 96)",
        "(101 << 102)",
        "(201 & 202)"
      )

  }

  test("Handle binary operators used as arguments to unary ops or method calls") {
    execute check ConversionCheck(
      //language=JavaScript
      """
        function f() {
           var a = -(1 + 2);
           var b = (3 + 4).toString;
        }
        """).required(
        "(1 + 2)",
        "(3 + 4)"
      ).forbidden(
        "(1 * 2)"
      )
  }

}
