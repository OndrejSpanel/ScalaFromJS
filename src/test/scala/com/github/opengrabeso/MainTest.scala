package com.github.opengrabeso

import JsonToString._
import Uglify._
import UglifyExt._

class MainTest extends org.scalatest.FunSuite {
  implicit class AnyExt(val value: Any) {
    def any: Any = value
  }

  test("Basic test") {
    val code = "answer = 42"
    val mCode = parse(code, defaultOptions.parse)
    assert(mCode.body.nonEmpty)
  }


  test("Parsing test") {
    val code = "answer = 42"

    val u = uglify(code)
    assert(u == "answer=42;")

    val m = parse(code, defaultOptions.parse)
    assert(m.start.pos == 0)
    assert(m.end.endpos == code.length)
    m.body.head match {
      case s: AST_SimpleStatement =>
        s.body match {
          case a: AST_Assign =>
            assert(a.left.start.`type` == "name")
            assert(a.left.name == "answer")
            assert(a.operator == "=")
            assert(a.right.start.`type` == "num")
            assert(a.right.start.value == 42.any)
        }

    }

  }



}
