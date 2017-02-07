import scala.scalajs.js
import scala.scalajs.js.annotation.JSExport
import Uglify._
import UglifyExt._
import JsonToString._

object Main extends js.JSApp {
  def tests(): Unit = {
    val code = "answer = 42"
    val m = parse(code, defaultOptions.parse)
    println(s"parse: ${m.json}")

    val u = uglify(code)
    assert(u == "answer=42;")

  }


  @JSExport
  def main(): Unit = {
    println("Testing ...")
    tests()
    println("Tests done")

  }
}
