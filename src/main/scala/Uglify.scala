import scala.scalajs.js
import scala.scalajs.js.annotation.{JSExport, JSGlobalScope, ScalaJSDefined}
import js.Dynamic.literal
import scala.scalajs.js.RegExp

@JSExport("Config")
@ScalaJSDefined
object Config extends js.Object {

  @ScalaJSDefined
  class Parse extends js.Object {
    var strict: Boolean = false
  }

  @ScalaJSDefined
  class Compress extends js.Object {
    var sequences: Boolean = true
    var properties: Boolean = true
    var dead_code: Boolean = true
    var drop_debugger: Boolean = true
    var unsafe: Boolean = true
    var unsafe_comps: Boolean = true
    var conditionals: Boolean = true
    var comparisons: Boolean = true
    var evaluate: Boolean = true
    var booleans: Boolean = true
    var loops: Boolean = true
    var unused: Boolean = true
    var hoist_funs: Boolean = true
    var hoist_vars: Boolean = false
    var if_return: Boolean = true
    var join_vars: Boolean = true
    var cascade: Boolean = true
    var side_effects: Boolean = true
    var negate_iife: Boolean = true
    var screw_ie8: Boolean = false
    var warnings: Boolean = true
    var global_defs: js.Dynamic = literal()
  }

  @ScalaJSDefined
  class Output extends js.Object {
    var indent_start  : Int = 0
    var indent_level  : Int = 4
    var quote_keys    : Boolean = false
    var space_colon   : Boolean = true
    var ascii_only    : Boolean = false
    var inline_script : Boolean = true
    var width         : Int = 80
    var max_line_len  : Int = 32000
    var beautify      : Boolean = false
    var source_map    : js.Dynamic = null
    var bracketize    : Boolean = false
    var semicolons    : Boolean = true
    var comments      : RegExp = RegExp("@license|@preserve|^!")
    var preserve_line : Boolean = false
    var screw_ie8     : Boolean = false
  }

  @ScalaJSDefined
  class Options extends js.Object {
    var parse: Parse = new Parse
    var compress: Compress = new Compress
    var output: Output = new Output
  }

  val default = new Options
}

@JSGlobalScope
@js.native
object Uglify extends js.Object {

  @js.native
  class Compressor(options: Config.Compress) extends js.Object

  @js.native
  class AST extends js.Object {
    def figure_out_scope(): Unit = js.native
    def transform(c: Compressor): AST = js.native
    def compute_char_frequency(): Unit = js.native
    def mangle_names(): Unit = js.native

    def print_to_string(config: Config.Output): String = js.native
  }

  def parse(code: String, options: js.Any): AST = js.native


}

import Uglify._

object UglifyExt {
  def uglify(code: String, options: Config.Options): String = {

    // 1. Parse
    var toplevel_ast = parse(code, options)
    toplevel_ast.figure_out_scope()

    // 2. Compress
    var compressor = new Compressor(options.compress)
    var compressed_ast = toplevel_ast.transform(compressor)

    // 3. Mangle
    compressed_ast.figure_out_scope()
    compressed_ast.compute_char_frequency()
    compressed_ast.mangle_names()

    // 4. Generate output
    val outCode = compressed_ast.print_to_string(options.output)

    outCode
  }

}
