import scala.scalajs.js
import scala.scalajs.js.annotation.JSExport
import Uglify._
import UglifyExt._

object Main extends js.JSApp {
  implicit class AnyExt(val value: Any) {
    def any: Any = value
  }

  def tests(): Unit = {
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


  @JSExport
  def main(): Unit = {
    println("Testing ...")
    tests()
    println("Tests done")

  }
}
