package com.github.opengrabeso

import Esprima._
import ESTree._
import JsonToString._

class MainTest extends org.scalatest.FunSuite {
  test("Tokenize simple expression") {
    val code = "answer = 42"
    val tokens = tokenize(code)
    assert(tokens.json == "[{\"type\":\"Identifier\",\"value\":\"answer\"},{\"type\":\"Punctuator\",\"value\":\"=\"},{\"type\":\"Numeric\",\"value\":\"42\"}]")
  }
  test("Parse simple expression") {
    val code = "answer = 42"
    val tree = parse(code)
    tree match {
      case n: Program =>
        assert(n.sourceType == "script")
        info(n.json)

    }
  }
}
