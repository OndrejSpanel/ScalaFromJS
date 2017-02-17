package com.github.opengrabeso

import org.scalatest.FunSuite
import Resources.{getResource => rsc}

class BasicConversionTests extends FunSuite with TestUtils {


  test("Simple functions") {
    ConversionCheck(
      rsc("simpleFunction/simpleFunctions.js"),
      Seq(
        "def firstFunction()",
        "def secondFunction()"
      ),
      ConversionCheck.standardForbidden ++ Seq(
        "function"
      )
    )
  }

  test("Function parameters and calls") {
    ConversionCheck(
      rsc("simpleFunction/callFunction.js"),
      Seq(
        "full = first + last",
        """result = concatenate("Zara", "Ali")""",
        "def concatenate(",
        "def secondFunction()"
      ),
      ConversionCheck.standardForbidden ++ Seq(
        "function"
      )
    )
  }

  test("Flow control") {
    ConversionCheck(
      rsc("control/control.js"),
      Seq(
      "if (b) {",
      "a += 1",
      "if (!b)",
      "else {",
      "for (i <- 0 until 3)"
      ),
      ConversionCheck.standardForbidden ++ Seq(
        "if (x) if (y)"
      )
    )
  }

  test("For loop special form") {
    ConversionCheck(
      rsc("control/for.js"),
      Seq(
        "for (a <- 0 until 10)",
        "while (s < 10)",
        "while (d < 10)",
        "var c = 0",
        "var d = 0",
        "c += 1",
        "d += 1",
        "s += 1"
      ),
      ConversionCheck.standardForbidden ++ Seq(
        "for (b <-",
        "for (c <-",
        "for (d <-"
      )
    )
  }

  test("Unsupported code") {
    ConversionCheck(
      rsc("unsupported/unsupported.js"),
      Seq(
        "/* Unsupported: Break */ break"
      ),
      Seq()
    )
  }

  test("String escaping") {
    ConversionCheck(
      """"Multiple lines\nAnd some tabs\tas well\r\n"""",
      Seq("\\n", "\\r", "\\t"),
      Seq("\t")
    )
  }

  test("Indenting") {
    val result = Main.convert(rsc("control/indent.js"))
    assert(result == normalizeEol(rsc("control/indent.scala")))
  }

  test("Simple class") {
    pending
    ConversionCheck(
      rsc("simpleClass/simpleClass.js"),
      Seq(
        "class Person",
        """var person = new Person("Bob", "M")"""
      ),
      ConversionCheck.standardForbidden ++ Seq(
        ".prototype."
      )
    )
  }

  test("Reserved words") {
    ConversionCheck(
      """function def()
         |{
         |    var val;
         |    var match;
         |    return yield
         |}""".stripMargin,
      "`def`","`val`","`match`","`yield`"
    )
  }

}
