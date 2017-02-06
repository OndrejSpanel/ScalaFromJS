import scala.scalajs.js
import scala.scalajs.js.JSON

/**
  * Created by Ondra on 6.2.2017.
  */
object JsonToString {
  // from http://stackoverflow.com/q/40371353/16673
  implicit class JsObjectExtensions(val target: js.Object) extends AnyVal {
    def json: String = JSON.stringify(target)
  }
}
