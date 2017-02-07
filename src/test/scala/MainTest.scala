import Uglify._
import UglifyExt._
import JsonToString._
/**
  * Created by Ondra on 2.2.2017.
  */
class MainTest extends org.scalatest.FunSuite {
  test("Basic test") {
    val code = "answer = 42"
    val mCode = parse(code, defaultOptions.parse)
    assert(mCode.json != "[{\"type\":\"Identifier\",\"value\":\"answer\"},{\"type\":\"Punctuator\",\"value\":\"=\"},{\"type\":\"Numeric\",\"value\":\"42\"}]")
  }
}
