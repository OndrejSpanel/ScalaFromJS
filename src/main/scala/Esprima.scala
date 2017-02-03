import scala.scalajs.js
import scala.scalajs.js.JSON
import scala.scalajs.js.annotation.JSName

object JsonToString {
  // from http://stackoverflow.com/q/40371353/16673
  implicit class JsObjectExtensions(val target: js.Object) extends AnyVal {
    def json: String = JSON.stringify(target)
  }
}

@JSName("esprima")
@js.native
object Esprima extends js.Object {

  @js.native
  class Config extends js.Object {
    // Annotate each node with its index-based location, default false
    var range: Boolean = js.native
    // Annotate each node with its column and row-based location, default false
    var loc: Boolean = js.native
    // Collect every line and block comment, default false
    // var comments: Boolean = js.native
  }

  @js.native
  class ParseConfig extends Config {
    // Define the program type: a script or a module, default: "script"
    var sourceType: String = js.native
    // Support JSX syntax, default false
    var jsx: Boolean = js.native
    // 	Tolerate a few cases of syntax errors, default false
    var tolerant: Boolean = js.native
    // Collect every token, default false
    var token: Boolean = js.native
  }

  @js.native
  class Token extends js.Object {
    val `type`: String = js.native
    val value: String = js.native

  }

  def tokenize(input: String, config: Config = js.native, delegate: String => String = js.native): js.Array[Token] = js.native

  def parse(input: String, config: ParseConfig = js.native): js.Dynamic = js.native
}
