package com.github.opengrabeso

import Esprima._
import JsonToString._

class MainTest extends org.scalatest.FunSuite {
  test("Basic test") {
    val code = "answer = 42"
    val tokens = tokenize(code)
    assert(tokens.json == "[{\"type\":\"Identifier\",\"value\":\"answer\"},{\"type\":\"Punctuator\",\"value\":\"=\"},{\"type\":\"Numeric\",\"value\":\"42\"}]")
  }
}
