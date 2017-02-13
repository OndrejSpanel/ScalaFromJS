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

  object ConversionCheck {
    val standardForbidden = Seq(";", "/* Unsupported:")

    def apply(code: String, mustHave: String*) = new ConversionCheck(code, mustHave)
  }

  case class ConversionCheck(code: String, mustHave: Seq[String], mustNotHave: Seq[String] = ConversionCheck.standardForbidden) {

    def checkResult(result: String): Try[Unit] = {
      val missing = mustHave.filter(!result.contains(_))
      val forbidden = mustNotHave.filter(result.contains(_))
      if (missing.isEmpty & forbidden.isEmpty) {
        info(result)
        Success(())
      } else Failure {
        def stringList(ss: Seq[String]) = ss.map("  " + _ + "\n").mkString

        val missingStr = if (missing.nonEmpty) "Missing: \n" + stringList(missing) else ""
        val forbiddenStr = if (forbidden.nonEmpty) "Forbidden: \n" + stringList(forbidden) else ""

        new UnsupportedOperationException(missingStr + forbiddenStr + "in \n" + result)
      }
    }

    def produceResult = {
      val ast = parse(code, defaultUglifyOptions.parse)
      val astOptimized = ast.optimize(defaultOptimizeOptions)
      ScalaOut.output(astOptimized, code)
    }

    val result = produceResult
    // TODO: better error reporting
    checkResult(result).failed.foreach(throw _)
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

  test("Simple functions") {
    ConversionCheck(
      rsc("simpleFunction/simpleFunctions.js"),
      Seq(
        "def firstFunction()",
        "def secondFunction()"
      ),
      ConversionCheck.standardForbidden ++ Seq(
        "function"
      )
    )
  }

  test("Function parameters and calls") {
    ConversionCheck(
      rsc("simpleFunction/callFunction.js"),
      Seq(
        "full = first + last",
        """result = concatenate("Zara", "Ali")""",
        "def concatenate(",
        "def secondFunction()"
      ),
      ConversionCheck.standardForbidden ++ Seq(
        "function"
      )
    )
  }

  test("Flow control") {
    ConversionCheck(
      rsc("control/control.js"),
      "if (b) {",
      "} while (!b)",
      "if (!b)",
      "else {",
      "for (i <- 0 until 3)"
    )
  }

  /*
  test("Simple class") {
    conversionTest(rsc("simpleClass/simpleClass.js"), rsc("simpleClass/simpleClass.scala"))
  }


  test("For loop special form") {
    conversionTest(rsc("control/for.js"), rsc("control/for.scala"))
  }

  test("Unsupported file handling") {
    conversionTest(rsc("unsupported/unsupported.js"), rsc("unsupported/unsupported.scala"))
  }
  */
}
