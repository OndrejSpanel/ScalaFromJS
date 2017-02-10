package com.github.opengrabeso

import Uglify._
import UglifyExt._
import utest._
import Resources.{getResource=>rsc}

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
        val code = rsc("answer42.js")
        val mCode = parse(code, defaultUglifyOptions.parse)
        assert(mCode.body.nonEmpty)
      }
      "Convert a file" - {
        def conversionTest(code: String, res: String) = {
          val ast = parse(code, defaultUglifyOptions.parse)
          val astOptimized = ast.optimize()
          val result = ScalaOut.output(astOptimized)
          assert(result == res)
        }

        "Simple functions" - {
          conversionTest(rsc("simpleFunction/simpleFunctions.js"), rsc("simpleFunction/simpleFunctions.scala"))
        }

        "Function parameters and calls" - {
          conversionTest(rsc("simpleFunction/callFunction.js"), rsc("simpleFunction/callFunction.scala"))
        }

        "Simple class" - {
          conversionTest(rsc("simpleClass/simpleClass.js"), rsc("simpleClass/simpleClass.scala"))
        }
      }

    }

  }
}
