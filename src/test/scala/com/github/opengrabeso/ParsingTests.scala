package com.github.opengrabeso

import com.github.opengrabeso.Uglify._
import com.github.opengrabeso.UglifyExt._
import org.scalatest.FunSuite
import Resources.{getResource => rsc}

class ParsingTests extends FunSuite with TestUtils {
  test("Basic test") {
    val code = "answer = 42"
    val mCode = parse(code, defaultUglifyOptions.parse)
    assert(mCode.body.nonEmpty)
  }


  test("Parsing test") {
    val code = "answer = 42"

    val u = uglify(code)
    assert(u == "answer=42;")

    val m = parse(code, defaultUglifyOptions.parse)
    assert(m.start.exists(_.pos == 0))
    assert(m.end.exists(_.endpos == code.length))
    (m.body.head: @unchecked) match {
      case s: AST_SimpleStatement =>
        (s.body: @unchecked) match {
          case a: AST_Assign =>
            assert(a.left.start.exists(_.`type` == "name"))
            assert(a.left.asInstanceOf[AST_SymbolRef].name == "answer")
            assert(a.operator == "=")
            assert(a.right.start.exists(_.`type` == "num"))
            assert(a.right.start.exists(_.value == 42.any))
        }

    }
  }

  test("Parse a file") {
    val code = rsc("answer42.js")
    val mCode = parse(code, defaultUglifyOptions.parse)
    assert(mCode.body.nonEmpty)
  }


}
