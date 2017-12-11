package com.github.opengrabeso

import com.github.opengrabeso.Uglify._
import com.github.opengrabeso.UglifyExt._
import org.scalatest.FunSuite

class ParsingTests extends FunSuite with TestUtils {
  test("Basic test") {
    val code = "answer = 42"
    val mCode = minify(code, defaultUglifyOptions).top
    assert(mCode.body.nonEmpty)
  }


  test("Parsing test") {
    val code = "answer = 42"

    val m = minify(code, defaultUglifyOptions).top
    assert(m.start.exists(_.pos == 0))
    assert(m.end.exists(_.endpos == code.length))
    (m.body.head: @unchecked) match {
      case s: Node.SimpleStatement =>
        (s.body: @unchecked) match {
          case a: Node.Assign =>
            assert(a.left.start.exists(_.`type` == "name"))
            assert(a.left.asInstanceOf[Node.SymbolRef].name == "answer")
            assert(a.operator == "=")
            assert(a.right.start.exists(_.`type` == "num"))
            assert(a.right.start.exists(_.value == 42.any))
        }

    }
  }

  test("Parse a resource file") {
    val code = rsc("answer42.js")
    val mCode = minify(code, defaultUglifyOptions).top
    assert(mCode.body.nonEmpty)
  }

  test("Walk AST") {
    val code = "answer = 42"

    val m = minify(code, defaultUglifyOptions).top
    var countStatements = 0
    m.walk {
      case s: Node.SimpleStatement =>
        countStatements += 1
        false
      case _ =>
        false
    }
    assert(countStatements == 1)
  }

  test("Transform AST") {
    val code = "answer = 42"

    val m = minify(code, defaultUglifyOptions).top
    var countStatements = 0
    m.transformAfter { (node, transformer) =>
      node match {
        case s: Node.SimpleStatement =>
          countStatements += 1
          node
        case _ =>
          node
      }
    }
    assert(countStatements == 1)
  }

}
