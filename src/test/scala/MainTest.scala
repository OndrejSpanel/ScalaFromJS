import Esprima._
import JsonToString._
/**
  * Created by Ondra on 2.2.2017.
  */
class MainTest extends org.scalatest.FunSuite {
  test("Basic test") {
    val code = "answer = 42"
    val tokens = tokenize(code)
    assert(tokens.json == "[{\"type\":\"Identifier\",\"value\":\"answer\"},{\"type\":\"Punctuator\",\"value\":\"=\"},{\"type\":\"Numeric\",\"value\":\"42\"}]")
  }
}
