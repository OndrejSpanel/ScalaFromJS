import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport

@JSImport("uglify-js", JSImport.Default)
@js.native
object Uglify extends js.Object {

  def parse(code: String): js.Object = js.native
}
