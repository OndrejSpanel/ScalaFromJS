import scala.scalajs.js
import scala.scalajs.js.annotation.JSExport
import Esprima._
import JsonToString._

object Main extends js.JSApp {
  @JSExport
  def main(): Unit = {
    println("Testing ...")
    val code = "answer = 42"
    val tokens = tokenize(code)
    println(s"Tokens: ${tokens.json}")
    val parsed = parse(code)
    println(s"Parsed: ${parsed.json}")
    println("Done.")
  }
}
