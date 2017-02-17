package com.github.opengrabeso

import org.scalatest.FunSuite
import Resources.{getResource => rsc}

class InferenceTests extends FunSuite with TestUtils {
  test("Indenting") {
    val result = Main.convert(rsc("control/indent.js"))
  }

  test("Detect vals") {
    ConversionCheck(
      rsc("inference/variables.js"),
      Seq(
        "val i = 1",
        "val d = 1.2",
        "val i = 1"
      ),
      ConversionCheck.standardForbidden ++ Seq(
        "function"
      )
    )
  }

  test("Infer var type") {
    ConversionCheck(
      rsc("inference/variables.js"),
      Seq(
        "var ii = 1",
        "val di = 1.2",
        "var aa : Any = 1"
      ), ConversionCheck.standardForbidden
    )
  }

}
