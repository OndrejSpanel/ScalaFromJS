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
        assert(n.body.length == 1)
        val n0 = n.body(0)
        info(n0.json)
        assert(n0.`type` == "ExpressionStatement")
        val es = n0.asInstanceOf[ExpressionStatement]
        assert(es.expression.`type` == "AssignmentExpression")
        val ae = es.expression.asInstanceOf[AssignmentExpression]
        assert(ae.`operator` == "=")
        assert(ae.left.`type` == "Identifier")
        assert(ae.right.`type` == "Literal")
        assert(ae.left.asInstanceOf[Identifier].name == "answer")
        //noinspection ComparingUnrelatedTypes
        assert(ae.right.asInstanceOf[Literal].value.toString == "42")

    }
  }
}
