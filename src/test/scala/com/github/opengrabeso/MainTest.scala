package com.github.opengrabeso

import Uglify._
import UglifyExt._
import org.scalatest.FunSuite
import Resources.{getResource => rsc}

import scala.util.{Failure, Success, Try}

class MainTest extends FunSuite {

  implicit class AnyExt(val value: Any) {
    def any: Any = value
  }

  case class ConversionCheck(code: String, mustHave: Seq[String], mustNotHave: Seq[String]) {
    def checkResult(result: String): Try[Unit] = {
      val missing = mustHave.filter(!result.contains(_))
      val forbidden = mustNotHave.filter(result.contains(_))
      if (missing.isEmpty & forbidden.isEmpty) Success(())
      else Failure {
        def stringList(ss: Seq[String]) = ss.map("  " + _ + "\n").mkString

        val missingStr = if (missing.nonEmpty) "Missing: \n" + stringList(missing) else ""
        val forbiddenStr = if (missing.nonEmpty) "Forbidden: \n" + stringList(missing) else ""

        new UnsupportedOperationException(missingStr + forbiddenStr)
      }
    }

    def check(): Unit = {
      val ast = parse(code, defaultUglifyOptions.parse)
      val astOptimized = ast.optimize(defaultOptimizeOptions)
      val result = ScalaOut.output(astOptimized, code)

      // TODO: better error reporting
      checkResult(result).failed.foreach(throw _)
    }
  }

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
    assert(m.start.pos == 0)
    assert(m.end.endpos == code.length)
    (m.body.head: @unchecked) match {
      case s: AST_SimpleStatement =>
        (s.body: @unchecked) match {
          case a: AST_Assign =>
            assert(a.left.start.`type` == "name")
            assert(a.left.asInstanceOf[AST_SymbolRef].name == "answer")
            assert(a.operator == "=")
            assert(a.right.start.`type` == "num")
            assert(a.right.start.value == 42.any)
        }

    }
  }

  test("Parse a file") {
    val code = rsc("answer42.js")
    val mCode = parse(code, defaultUglifyOptions.parse)
    assert(mCode.body.nonEmpty)
  }

  def conversionTest(code: String, res: String) = {
    val ast = parse(code, defaultUglifyOptions.parse)
    val astOptimized = ast.optimize(defaultOptimizeOptions)
    val result = ScalaOut.output(astOptimized, code)
    assert(result == res)
  }

  test("Simple functions") {
    ConversionCheck(
      rsc("simpleFunction/simpleFunctions.js"),
      Seq(
        "def firstFunction()",
        "def secondFunction()"
      ),
      Seq(
        "function"
      )
    ).check()
  }

  test("Function parameters and calls") {
    ConversionCheck(
      rsc("simpleFunction/callFunction.js"),
      Seq(
        "full = first + last",
        """result = concatenate("Zara", "Ali")""",
        "def "
      ),
      Seq(
        "def concatenate(",
        "def secondFunction()"
      )
    )
  }

  /*
  test("Simple class") {
    conversionTest(rsc("simpleClass/simpleClass.js"), rsc("simpleClass/simpleClass.scala"))
  }

  test("Flow control") {
    conversionTest(rsc("control/control.js"), rsc("control/control.scala"))
  }

  test("For loop special form") {
    conversionTest(rsc("control/for.js"), rsc("control/for.scala"))
  }

  test("Unsupported file handling") {
    conversionTest(rsc("unsupported/unsupported.js"), rsc("unsupported/unsupported.scala"))
  }
  */
}
