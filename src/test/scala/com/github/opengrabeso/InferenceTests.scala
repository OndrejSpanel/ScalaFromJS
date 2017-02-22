package com.github.opengrabeso

import org.scalatest.FunSuite
import Resources.{getResource => rsc}

class InferenceTests extends FunSuite with TestUtils {
  test("Detect vals") {
    // note: overlaps expression/variables (ExpressionTests / "Val detection")
    execute check ConversionCheck(rsc("inference/variables.js"))
      .required(
        "val i = 1",
        "val d = 1.2",
        "val i = 1"
      )
      .forbidden("function")
  }

  test("Infer var type") {
    pending
    execute check ConversionCheck(rsc("inference/variables.js"))
      .required(
        "var ii = 1",
        "val di = 1.2",
        "var aa : Any = 1"
      )
  }

}
