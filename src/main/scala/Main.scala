import scala.scalajs.js
import scala.scalajs.js.annotation.JSExport
import Uglify._
import JsonToString._

object Main extends js.JSApp {
  @JSExport
  def main(): Unit = {
    println("Testing ...")
    val code = "answer = 42"
    val m = parse(code)
    println(s"minify: $m")
  }
}
