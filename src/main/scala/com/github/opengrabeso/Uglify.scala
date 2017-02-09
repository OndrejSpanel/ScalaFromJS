package com.github.opengrabeso

import scala.scalajs.js
import scala.scalajs.js.RegExp
import scala.scalajs.js.annotation.{JSImport, ScalaJSDefined}

@JSImport("uglify-js", JSImport.Namespace)
@js.native
object Uglify extends js.Object {
  // http://lisperator.net/uglifyjs/ast
  @js.native
  class AST_Token extends js.Object {
    val col: Int = js.native
    val line: Int = js.native
    val endcol: Int = js.native
    val endline: Int = js.native

    val pos: Int = js.native
    val endpos: Int = js.native

    val comments_before: js.Array[js.Any] = js.native
    val file: js.Any = js.native

    val nlb: Boolean = js.native
    val raw: String = js.native
    val `type`: String = js.native
    val value: js.Any = js.native
  }

  @js.native
  sealed class AST_Node extends js.Object {
    val start: AST_Token = js.native
    val end: AST_Token = js.native
  }

  @js.native
  sealed abstract class AST_Statement extends AST_Node

  @js.native
  class AST_SimpleStatement extends AST_Statement {
    val body: AST_Node = js.native // [AST_Node] an expression node (should not be instanceof AST_Statement)
  }

  @js.native
  class AST_Block extends AST_Statement {
    val body: js.Array[AST_Statement] = js.native

  }

  @js.native
  class AST_Binary extends AST_Node {
    val left: AST_Node = js.native // [AST_Node] left-hand side expression
    val operator: String = js.native // [string] the operator
    val right: AST_Node = js.native // [AST_Node] right-hand side expression
  }

  @js.native
  class AST_Symbol extends AST_Node {
    val name: String = js.native // "[string] name of this symbol",
    val scope: js.UndefOr[AST_Scope] = js.native // "[AST_Scope/S] the current scope (not necessarily the definition scope)",
    val thedef: js.UndefOr[js.Any] = js.native // "[SymbolDef/S] the definition of this symbol"

  }

  @js.native
  class AST_SymbolRef extends AST_Symbol

  @js.native
  class AST_Assign extends AST_Binary {
    override val left: AST_SymbolRef = js.native
    override val right: AST_Node = js.native // TODO: more restrictive type should be possible
  }

  @js.native
  class AST_BlockStatement extends AST_Block

  @js.native
  class AST_EmptyStatement extends AST_Statement

  @js.native
  class AST_StatementWithBody extends AST_Statement {
    val body: AST_Statement = js.native
  }

  @js.native
  class AST_Scope extends AST_Block {
    val directives: js.UndefOr[js.Array[String]] = js.native // [string*/S] an array of directives declared in this scope
    val variables: js.Dynamic = js.native // [Object/S] a map of name -> SymbolDef for all variables/functions defined in this scope
    val functions: js.Dynamic = js.native // [Object/S] like `variables`, but only lists function declarations
    val uses_with: js.UndefOr[Boolean] = js.native // [boolean/S] tells whether this scope uses the `with` statement
    val uses_eval: js.UndefOr[Boolean] = js.native // [boolean/S] tells whether this scope contains a direct call to the global `eval`
    val parent_scope: js.UndefOr[AST_Scope] = js.native // [AST_Scope?/S] link to the parent scope
    val enclosed: js.Dynamic  = js.native // [SymbolDef*/S] a list of all symbol definitions that are accessed from this scope or any subscopes
    val cname: js.UndefOr[Int]  = js.native // "[integer/S] current index for mangling variables (used internally by the mangler)
  }

  @js.native
  class AST_Toplevel extends AST_Scope {
    def figure_out_scope(): Unit = js.native
    def transform(c: Compressor): AST_Toplevel = js.native
    def compute_char_frequency(): Unit = js.native
    def mangle_names(): Unit = js.native

    def print_to_string(config: UglifyExt.Options.Output): String = js.native
  }


  @js.native
  class Compressor(options: UglifyExt.Options.Compress) extends js.Object

  def parse(code: String, options: js.Any): AST_Toplevel = js.native


}

object UglifyExt {
  import Uglify._

  @ScalaJSDefined
  class Parse extends js.Object {
    var strict: Boolean = false
  }

  object Options {

    @ScalaJSDefined
    class Compress extends js.Object {
      // https://github.com/mishoo/UglifyJS2#compressor-options
      var sequences: Boolean = true //  join consecutive simple statements using the comma operator
      var properties: Boolean = true //  rewrite property access using the dot notation
      var dead_code: Boolean = true //  remove unreachable code
      var drop_debugger: Boolean = true // remove debugger; statements
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
      var global_defs: Map[String, Any] = Map.empty
    }

    @ScalaJSDefined
    class Output extends js.Object {
      var indent_start: Int = 0
      var indent_level: Int = 4
      var quote_keys: Boolean = false
      var space_colon: Boolean = true
      var ascii_only: Boolean = false
      var inline_script: Boolean = true
      var width: Int = 80
      var max_line_len: Int = 32000
      var beautify: Boolean = false
      var source_map: js.Dynamic = null
      var bracketize: Boolean = false
      var semicolons: Boolean = true
      var comments: RegExp = RegExp("@license|@preserve|^!")
      var preserve_line: Boolean = false
      var screw_ie8: Boolean = false
    }

  }

  @ScalaJSDefined
  class Options extends js.Object {
    import Options._

    val parse: Parse = new Parse
    val compress: Compress = new Compress
    val output: Output = new Output
  }

  val defaultUglifyOptions = new Options

  // options for reasonable optimization
  val defaultOptimizeOptions = new Options.Compress {
    sequences = false
    join_vars = false
    hoist_vars = true
  }
  val defaultOutputOptions = new Options.Output {
    beautify = true
  }

  implicit class AST_ToplevelOps(val ast: AST_Toplevel) {
    def optimize(options: Options.Compress = defaultOptimizeOptions): AST_Toplevel = {
      ast.figure_out_scope()

      val compressor = new Compressor(options)
      val compressed_ast = ast.transform(compressor)

      compressed_ast.figure_out_scope()
      compressed_ast
    }

    def source(options: Options.Output = defaultOutputOptions): String = {
      ast.print_to_string(options)
    }

    def mangleNames(): AST_Toplevel = {
      ast.figure_out_scope()
      ast.compute_char_frequency()
      ast.mangle_names()
      ast
    }

  }

  def uglify(code: String, options: Options = defaultUglifyOptions): String = {

    val toplevel_ast = parse(code, options)

    val compressed_ast = toplevel_ast.optimize(options.compress)

    // 3. Mangle
    compressed_ast.mangleNames()

    // 4. Generate output
    val outCode = compressed_ast.source(options.output)

    outCode
  }



}
