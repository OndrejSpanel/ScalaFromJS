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
        val mCode = parse(code, defaultUglifyOptions.parse)
        assert(mCode.body.nonEmpty)
      }

      "Parsing test" - {
        implicit class AnyExt(val value: Any) {
          def any: Any = value
        }

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
                assert(a.left.name == "answer")
                assert(a.operator == "=")
                assert(a.right.start.`type` == "num")
                assert(a.right.start.value == 42.any)
            }

        }
      }
    }

    "Test files" - {
      "Parse a file" - {
        _root_.`answer42.js`
        textResource("answer42.js").map { code =>
          val mCode = parse(code, defaultUglifyOptions.parse)
          assert(mCode.body.nonEmpty)
        }
      }

      "Convert a file" - {
        def fileTestData(source: String, result: String) = {
          val data = Future.sequence(Seq(textResource(source), textResource(result)))
          data.map { case sData +: rData +: Seq() =>
            (sData, rData)
          }
        }

        def conversionTest(sourceFile: String, resultFile: String): Future[Unit] = {
          val testData = fileTestData(sourceFile, resultFile)
          testData.map { case (code, res) =>
            val ast = parse(code, defaultUglifyOptions.parse)
            val astOptimized = ast.optimize()
            val result = ScalaOut.output(astOptimized)
            println(result)
            assert(result == res)
          }


        }

        "Simple functions" - {
          conversionTest("simpleFunction/simpleFunctions.js", "simpleFunction/simpleFunctions.scala")
        }

        "Function parameters and calls" - {
          conversionTest("simpleFunction/callFunction.js", "simpleFunction/callFunction.scala")
        }

        "Simple class" - {
          conversionTest("simpleClass/simpleClass.js", "simpleClass/simpleClass.scala")
        }
      }

    }

  }
}
