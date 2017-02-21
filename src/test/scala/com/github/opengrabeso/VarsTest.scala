package com.github.opengrabeso

import Resources.{getResource => rsc}

import org.scalatest.FunSuite

class VarsTest extends FunSuite with TestUtils {

  test("Val detection") {
    execute check ConversionCheck(rsc("variables/variables.js")).
    required(
      "val s =",
      "var x =",
      "var y",
      "var z"
    ).forbidden(
      "val x",
      "val y",
      "var y =",
      "var z ="
    )
  }
}
