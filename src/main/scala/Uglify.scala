import scala.scalajs.js
import scala.scalajs.js.annotation.{JSExport, JSGlobalScope}
import js.Dynamic.literal
import scala.scalajs.js.RegExp

@JSExport("Options")
object Options {
  val default = literal (
    parse = literal(
      strict = false
    ),

    compress = literal (
      sequences     = true,
      properties    = true,
      dead_code     = true,
      drop_debugger = true,
      unsafe        = true,
      unsafe_comps  = true,
      conditionals  = true,
      comparisons   = true,
      evaluate      = true,
      booleans      = true,
      loops         = true,
      unused        = true,
      hoist_funs    = true,
      hoist_vars    = false,
      if_return     = true,
      join_vars     = true,
      cascade       = true,
      side_effects  = true,
      negate_iife   = true,
      screw_ie8     = false,
      warnings      = true,
      global_defs   = literal()
    ),
    output = literal (
      indent_start  = 0,
      indent_level  = 4,
      quote_keys    = false,
      space_colon   = true,
      ascii_only    = false,
      inline_script = true,
      width         = 80,
      max_line_len  = 32000,
      beautify      = false,
      source_map    = null,
      bracketize    = false,
      semicolons    = true,
      comments      = RegExp("@license|@preserve|^!"),
      preserve_line = false,
      screw_ie8     = false
    )
  )
}

@JSGlobalScope
@js.native
object Uglify extends js.Object {

  def uglify(code: String, options: js.Any): js.Object = js.native

  def parse(code: String, options: js.Any): js.Object = js.native
}
