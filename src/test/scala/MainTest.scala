import Uglify._
import JsonToString._
/**
  * Created by Ondra on 2.2.2017.
  */
class MainTest extends org.scalatest.FunSuite {
  test("Basic test") {
    val code = "answer = 42"
    val mCode = minify(code)
    assert(mCode == "[{\"type\":\"Identifier\",\"value\":\"answer\"},{\"type\":\"Punctuator\",\"value\":\"=\"},{\"type\":\"Numeric\",\"value\":\"42\"}]")
  }
}
