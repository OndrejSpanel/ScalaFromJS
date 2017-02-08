import Uglify._
import UglifyExt._
import JsonToString._
/**
  * Created by Ondra on 2.2.2017.
  */
class MainTest extends org.scalatest.FunSuite {
  implicit class AnyExt(val value: Any) {
    def any: Any = value
  }

  test("Basic test") {
    val code = "answer = 42"
    val mCode = parse(code, defaultOptions.parse)
    assert(mCode.json != "[{\"type\":\"Identifier\",\"value\":\"answer\"},{\"type\":\"Punctuator\",\"value\":\"=\"},{\"type\":\"Numeric\",\"value\":\"42\"}]")
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
