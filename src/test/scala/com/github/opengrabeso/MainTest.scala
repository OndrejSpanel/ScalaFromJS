package com.github.opengrabeso

import Uglify._
import UglifyExt._
import utest._
import TestUtils._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

object MainTest extends TestSuite {

  val tests = this {
    "Short expressions" - {
      "Basic test" - {
        val code = "answer = 42"
        val mCode = parse(code, defaultOptions.parse)
        assert(mCode.body.nonEmpty)
      }

      "Parsing test" - {
        implicit class AnyExt(val value: Any) {
          def any: Any = value
        }

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

    "Test files" - {
      def fileTestData(source: String, result: String) = {
        val data = Future.sequence(Seq(textResource(source), textResource(result)))
        data.map { case sData +: rData +: Seq() =>
          (sData, rData)
        }
      }

      "Parse a file" - {
        textResource("answer42.js").map { code =>
          val mCode = parse(code, defaultOptions.parse)
          assert(mCode.body.nonEmpty)
        }
      }

      "Convert a file" - {
        "Simple functions" - {
          textResource("simpleFunction/simpleFunctions.js").map { code =>
            val mCode = parse(code, defaultOptions.parse)
            assert(mCode.body.nonEmpty)
          }
        }

        "Function parameters and calls" - {
          val testData = fileTestData("simpleFunction/simpleFunctions.js", "simpleFunction/simpleFunctions.scala")
          testData.map { case (code, res) =>
            val mCode = parse(code, defaultOptions.parse)
            assert(mCode.body.nonEmpty)
          }
        }

        "Simple class" - {
          val testData = fileTestData("simpleClass/simpleClass.js", "simpleClass/simpleClass.scala")
          testData.map { case (code, res) =>
            val mCode = parse(code, defaultOptions.parse)
            assert(mCode.body.nonEmpty)
          }

        }
      }

    }

  }
}
