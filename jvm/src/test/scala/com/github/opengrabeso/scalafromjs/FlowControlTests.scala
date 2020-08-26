package com.github.opengrabeso.scalafromjs

import org.scalatest.funsuite.AnyFunSuite

class FlowControlTests extends AnyFunSuite with TestUtils {


  test("Flow control") {
    exec check ConversionCheck(rsc("control/control.js"))
      .required(
        "if (b) {",
        "a += 1",
        "if (!b)",
        "else {",
        "for (i <- 0 until 3)"
      ).forbidden("if (x) if (y)")
  }

  test("For loop special form") {
    exec check ConversionCheck(rsc("control/for.js"))
      .required(
        "for (a <- 0 until 10)",
        "for (i <- 0 until 2 + 3)",
        "while (s < 10)",
        "while (d < 10)",
        "var c = 0",
        "var d = 0",
        "c += 1",
        "d += 1",
        "s += 1"
      ).forbidden(
      "for (b <-",
      "for (c <-",
      "for (d <-"
    )
  }

  test("Switch / case / break") {
    exec check ConversionCheck(rsc("control/switch.js"))
      .required(
        "a match",
        "case \"A\" | \"B\" | \"C\" =>",
        "????(\"Missing break\")",
        "case \"E\" | \"F\" =>",
        "case _ =>"
      ).forbidden(
        "case \"G\""
      )

  }
}
