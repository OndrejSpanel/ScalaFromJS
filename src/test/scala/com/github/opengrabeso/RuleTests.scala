package com.github.opengrabeso

import org.scalatest.FunSuite

class RuleTests extends FunSuite with TestUtils {
  test("Delete member variables and functions") {
    execute check ConversionCheck(rsc("rules/deleteMembers.js"))
      .required(
        "def natural",
        "var natural"
      ).forbidden(
        "def exotic",
        "var exotic"
     )

  }
}
