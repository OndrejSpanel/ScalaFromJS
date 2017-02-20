package com.github.opengrabeso

import scala.language.implicitConversions
import scala.scalajs.js
import scala.scalajs.js.RegExp
import scala.scalajs.js.annotation.{JSImport, JSName, ScalaJSDefined}

@JSName("UglifyJS")
@js.native
object Uglify extends js.Object {

  // http://lisperator.net/uglifyjs/ast
  @js.native class AST_Token extends js.Object {
    val col: Int = js.native
    val line: Int = js.native
    val endcol: Int = js.native
    val endline: Int = js.native

    val pos: Int = js.native
    val endpos: Int = js.native

    val comments_before: js.Array[AST_Token] = js.native
    val file: js.Any = js.native

    val nlb: Boolean = js.native
    val raw: String = js.native
    val `type`: String = js.native
    val value: js.Any = js.native
  }

  @js.native class SymbolDef extends js.Object {
    // points to the AST_Scope where this is defined.
    var scope: AST_Scope = js.native
    // orig — an array of AST_SymbolDeclaration-s where this variable is defined
    var orig: js.Array[AST_SymbolDeclaration] = js.native
    // the original symbol name
    var name: String = js.native
    // an array of AST_SymbolRef nodes that were determined to point to this definition.
    var references: js.Array[AST_SymbolRef] = js.native
    // boolean, tells us if this is a global definition.
    var global: Boolean = js.native
    // the mangled name for this node, created by toplevel.mangle_names(). If present, the code generator will use this name when printing symbols.
    var mangled_name: js.Any = js.native
    // boolean, tells us if this is an undeclared definition (more below about undeclared names).
    var undeclared: Boolean = js.native
    // boolean, true if this is a constant definition (occurring in const).
    var constant: Boolean = js.native

    //var index: js.Any = js.native
    //var id: js.Any = js.native

    // our field: is value?
    var _isVal: js.UndefOr[Boolean] = js.native
    // inferred Scala type
    var _type: js.UndefOr[String] = js.native
    // ellided, merged with first assignment
    var _isEllided: js.UndefOr[Boolean] = js.native
  }



  // f:  return true to abort the walk
  @js.native class TreeWalker(f: js.Function2[AST_Node, js.Function0[Unit], Boolean]) extends js.Any

  @js.native class TreeTransformer(f: js.Function2[AST_Node, js.Function2[AST_Node, TreeTransformer, AST_Node], AST_Node]) extends js.Any

  @js.native sealed abstract class AST_Node extends js.Object {
    val start: js.UndefOr[AST_Token] = js.native
    val end: js.UndefOr[AST_Token] = js.native

    @JSName("walk")
    def walk_js(walker: TreeWalker): Unit = js.native

    @JSName("transform")
    def transform_js(transformer: TreeTransformer): AST_Node = js.native

    override def clone(): AST_Node = js.native
  }

  @js.native sealed abstract class AST_Statement extends AST_Node

  @js.native class AST_Debugger extends AST_Statement

  @js.native class AST_Directive extends AST_Statement

  @js.native class AST_SimpleStatement extends AST_Statement {
    var body: AST_Node = js.native // [AST_Node] an expression node (should not be instanceof AST_Statement)
  }

  @js.native class AST_Block extends AST_Statement {
    var body: js.Array[AST_Statement] = js.native
  }

  @js.native class AST_BlockStatement extends AST_Block

  @js.native class AST_Scope extends AST_Block {
    // [string*/S] an array of directives declared in this scope
    val directives: js.UndefOr[js.Array[String]] = js.native
    // [Object/S] a map of name -> SymbolDef for all variables/functions defined in this scope
    val variables: js.Dynamic = js.native
    // [Object/S] like `variables`, but only lists function declarations
    val functions: js.Dynamic = js.native
    // [boolean/S] tells whether this scope uses the `with` statement
    val uses_with: js.UndefOr[Boolean] = js.native
    // [boolean/S] tells whether this scope contains a direct call to the global `eval`
    val uses_eval: js.UndefOr[Boolean] = js.native
    // [AST_Scope?/S] link to the parent scope
    val parent_scope: js.UndefOr[AST_Scope] = js.native
    // [SymbolDef*/S] a list of all symbol definitions that are accessed from this scope or any subscopes
    val enclosed: js.Dynamic = js.native
    // [integer/S] current index for mangling variables (used internally by the mangler)
    val cname: js.UndefOr[Int] = js.native
  }

  @js.native class AST_Toplevel extends AST_Scope {
    def figure_out_scope(): Unit = js.native

    def transform(c: Compressor): AST_Toplevel = js.native

    def compute_char_frequency(): Unit = js.native

    def mangle_names(): Unit = js.native

    def print_to_string(config: UglifyExt.Options.Output): String = js.native
  }

  @js.native class AST_Lambda extends AST_Scope {
    val name: js.UndefOr[AST_SymbolDeclaration] = js.native
    //[AST_SymbolDeclaration?] the name of this function
    val argnames: js.Array[AST_SymbolFunarg] = js.native
    // [AST_SymbolFunarg*] array of function arguments
    val uses_arguments: js.UndefOr[Boolean] = js.native // "[boolean/S] tells whether this function accesses the arguments array"
  }

  @js.native class AST_Accessor extends AST_Lambda

  @js.native class AST_Function extends AST_Lambda

  @js.native class AST_Defun extends AST_Lambda

  @js.native class AST_Switch extends AST_Scope {
    val expression: AST_Node = js.native // [AST_Node] the `switch` “discriminant
  }

  @js.native sealed abstract class AST_SwitchBranch extends AST_Scope

  @js.native class AST_Default extends AST_SwitchBranch

  @js.native class AST_Case extends AST_SwitchBranch {
    // [AST_Node] the `case` expression
    val expression: AST_Node = js.native
  }

  @js.native class AST_Try extends AST_Scope {
    // [AST_Catch?] the catch block, or null if not present
    val bcatch: js.UndefOr[AST_Catch] = js.native
    // [AST_Finally?] the finally block, or null if not present
    val bfinally: js.UndefOr[AST_Finally] = js.native
  }

  @js.native class AST_Catch extends AST_Scope {
    // [AST_SymbolCatch] symbol for the exception
    val argname: AST_SymbolCatch = js.native
  }

  @js.native class AST_Finally extends AST_Scope

  @js.native class AST_EmptyStatement extends AST_Statement

  @js.native abstract sealed class AST_StatementWithBody extends AST_Statement {
    val body: AST_Statement = js.native
  }

  @js.native class AST_LabeledStatement extends AST_StatementWithBody {
    // [AST_Label] a label definition
    val label: AST_Label = js.native
  }

  @js.native sealed abstract class AST_IterationStatement extends AST_StatementWithBody

  @js.native sealed abstract class AST_DWLoop extends AST_IterationStatement {
    // [AST_Node] the loop condition.  Should not be instanceof AST_Statement
    val condition: AST_Node = js.native
  }

  @js.native class AST_Do extends AST_DWLoop
  @js.native class AST_While extends AST_DWLoop

  @js.native class AST_For extends AST_IterationStatement {
    // [AST_Node?] the `for` initialization code, or null if empty
    var init: js.UndefOr[AST_Node] = js.native
    // [AST_Node?] the `for` termination clause, or null if empty
    var condition: js.UndefOr[AST_Node] = js.native
    // [AST_Node?] the `for` update clause, or null if empty
    var step: js.UndefOr[AST_Node] = js.native
  }

  @js.native class AST_ForIn extends AST_IterationStatement {
    // [AST_Node] the `for/in` initialization code
    var init: AST_Node = js.native
    // [AST_SymbolRef?] the loop variable, only if `init` is AST_Var
    var name: js.UndefOr[AST_SymbolRef] = js.native
    // [AST_Node] the object that we're looping through
    var `object`: AST_Node = js.native
  }

  @js.native class AST_With extends AST_StatementWithBody {
    // [AST_Node] the `with` expression
    val expression: AST_Node  = js.native

  }

  @js.native class AST_If extends AST_StatementWithBody {
    // [AST_Node] the `if` condition
    val condition: js.UndefOr[AST_Node]  = js.native
    // [AST_Statement?] the `else` part, or null if not present
    val alternative: js.UndefOr[AST_Statement]  = js.native
  }

  @js.native sealed abstract class AST_Jump extends AST_Statement

  @js.native sealed abstract class AST_Exit extends AST_Jump {
    // [AST_Node?] the value returned or thrown by this statement; could be null for AST_Return
    val value: js.UndefOr[AST_Node]  = js.native
  }

  @js.native class AST_Return extends AST_Exit
  @js.native class AST_Throw extends AST_Exit

  @js.native sealed abstract class AST_LoopControl extends AST_Jump {
    // [AST_LabelRef?] the label, or null if none
    val label: js.UndefOr[AST_LabelRef] = js.native

  }
  @js.native class AST_Break extends AST_LoopControl
  @js.native class AST_Continue extends AST_LoopControl

  @js.native sealed abstract class AST_Definitions extends AST_Statement {
    // [AST_VarDef*] array of variable definitions
    val definitions: js.Array[AST_VarDef] = js.native
  }

  @js.native class AST_Var extends AST_Definitions
  @js.native class AST_Const extends AST_Definitions
  @js.native class AST_Let extends AST_Definitions

  @js.native class AST_VarDef extends AST_Node {
    // [AST_SymbolVar|AST_SymbolConst] name of the variable
    val name: AST_SymbolVarOrConst = js.native
    // "[AST_Node?] initializer, or null of there's no initializer"
    val value: js.UndefOr[AST_Node] = js.native
  }

  @js.native class AST_Call extends AST_Node {
    //[AST_Node] expression to invoke as function
    val expression: AST_Node = js.native
    //[AST_Node*] array of arguments
    val args: js.Array[AST_Node] = js.native
  }

  @js.native class AST_New extends AST_Call

  @js.native class AST_Seq extends AST_Node {
    // [AST_Node] first element in sequence
    val car: AST_Node = js.native
    // [AST_Node] second element in sequence
    val cdr: AST_Node = js.native
  }

  @js.native sealed abstract class AST_PropAccess extends AST_Node {
    // [AST_Node] the “container” expression
    val expression: AST_Node = js.native
    // [AST_Node|string] the property to access.  For AST_Dot this is always a plain string, while for AST_Sub it's an arbitrary AST_Node
    def property: Any = js.native
  }

  @js.native class AST_Dot extends AST_PropAccess {
    override def property: String = js.native
  }
  @js.native class AST_Sub extends AST_PropAccess {
    override val property: AST_Node = js.native
  }

  @js.native sealed abstract class AST_Unary extends AST_Node {
    // [string] the operator
    val operator: String = js.native
    // [AST_Node] expression that this unary operator applies to
    val expression: AST_Node = js.native
  }

  @js.native class AST_UnaryPrefix extends AST_Unary
  @js.native class AST_UnaryPostfix extends AST_Unary

  @js.native class AST_Binary extends AST_Node {
    // [AST_Node] left-hand side expression
    val left: AST_Node = js.native
    // [string] the operator
    val operator: String = js.native
    // [AST_Node] right-hand side expression
    val right: AST_Node = js.native
  }

  @js.native class AST_Assign extends AST_Binary

  @js.native class AST_Conditional extends AST_Node {
    // [AST_Node]
    val condition: AST_Node = js.native
    // [AST_Node]
    val consequent: AST_Node = js.native
    // [AST_Node]
    val alternative: AST_Node = js.native
  }

  @js.native class AST_Array extends AST_Node {
    // [AST_Node*] array of elements
    val elements: js.Array[AST_Node] = js.native
  }

  @js.native class AST_Object extends AST_Node {
    // [AST_ObjectProperty*] array of properties
    val properties: js.Array[AST_ObjectProperty] = js.native
  }

  @js.native sealed abstract class AST_ObjectProperty extends AST_Node {
    // [string] the property name converted to a string for ObjectKeyVal.  For setters and getters this is an arbitrary AST_Node.
    def key: Any = js.native
    // [AST_Node] property value.  For setters and getters this is an AST_Function.
    val value: AST_Node = js.native
  }

  @js.native class AST_ObjectKeyVal extends AST_ObjectProperty {
    override def key: String = js.native

    // [string] the original quote character
    val quote: String =  js.native
  }

  @js.native class AST_ObjectSetterOrGetter extends AST_ObjectProperty {
    // [string] the property name converted to a string for ObjectKeyVal.  For setters and getters this is an arbitrary AST_Node.
    override def key: AST_Node = js.native
    // [AST_Node] property value.  For setters and getters this is an AST_Function.
    override val value: AST_Function = js.native
  }
  @js.native class AST_ObjectSetter extends AST_ObjectSetterOrGetter
  @js.native class AST_ObjectGetter extends AST_ObjectSetterOrGetter

  @js.native sealed class AST_Symbol extends AST_Node {
    // [string] name of this symbol
    val name: String = js.native
    // [AST_Scope/S] the current scope (not necessarily the definition scope)
    val scope: js.UndefOr[AST_Scope] = js.native
    // [SymbolDef/S] the definition of this symbol
    val thedef: js.UndefOr[SymbolDef] = js.native
  }

  @js.native class AST_SymbolAccessor extends AST_Symbol
  @js.native class AST_SymbolDeclaration extends AST_Symbol {
    // [AST_Node*/S] array of initializers for this declaration.
    val init: js.UndefOr[js.Array[AST_Node]] = js.native
  }

  @js.native class AST_SymbolVarOrConst extends AST_SymbolDeclaration
  @js.native class AST_SymbolVar extends AST_SymbolVarOrConst
  @js.native class AST_SymbolFunarg extends AST_SymbolVar
  @js.native class AST_SymbolConst extends AST_SymbolVarOrConst

  @js.native class AST_SymbolDefun extends AST_SymbolDeclaration
  @js.native class AST_SymbolLambda extends AST_SymbolDeclaration
  @js.native class AST_SymbolCatch extends AST_SymbolDeclaration

  @js.native class AST_Label extends AST_Symbol {
    // [AST_LoopControl*] a list of nodes referring to this label
    val references: js.Array[AST_LoopControl] = js.native
  }

  @js.native class AST_SymbolRef extends AST_Symbol {
    // our extension: merge init into it
    var _mergedInit: js.UndefOr[Boolean]= js.native
  }
  @js.native class AST_LabelRef extends AST_Symbol
  @js.native class AST_This extends AST_Symbol

  @js.native sealed abstract class AST_Constant extends AST_Symbol

  @js.native class AST_String extends AST_Constant {
    // [string] the contents of this string
    val value: String = js.native
    // [string] the original quote character
    val quote: String  = js.native
  }
  @js.native class AST_Number extends AST_Constant {
    // [number] the numeric value
    val value: Double = js.native
    // [string] numeric value as string (optional)
    val literal: js.UndefOr[String] = js.native
  }

  @js.native class AST_RegExp extends AST_Constant {
    // [RegExp] the actual regexp
    val value: RegExp  = js.native
  }

  @js.native sealed abstract class AST_Atom extends AST_Constant
  @js.native class AST_Null extends AST_Atom
  @js.native class AST_NaN extends AST_Atom
  @js.native class AST_Undefined extends AST_Atom
  @js.native class AST_Hole extends AST_Atom
  @js.native class AST_Infinity extends AST_Atom
  @js.native abstract class AST_Boolean extends AST_Atom
  @js.native class AST_False extends AST_Boolean
  @js.native class AST_True extends AST_Boolean

  @js.native
  class Compressor(options: UglifyExt.Options.Compress) extends js.Object

  def parse(code: String, options: UglifyExt.Options.Parse): AST_Toplevel = js.native


}

object UglifyExt {

  import Uglify._


  object Options {

    @ScalaJSDefined
    class Parse extends js.Object {
      var strict: Boolean = false
    }

    @ScalaJSDefined
    class Compress extends js.Object {
      // https://github.com/mishoo/UglifyJS2#compressor-options
      var sequences: Boolean = true
      //  join consecutive simple statements using the comma operator
      var properties: Boolean = true
      //  rewrite property access using the dot notation
      var dead_code: Boolean = true
      //  remove unreachable code
      var drop_debugger: Boolean = true
      // remove debugger; statements
      var unsafe: Boolean = true
      var unsafe_comps: Boolean = true
      var conditionals: Boolean = false
      var comparisons: Boolean = true
      var evaluate: Boolean = true
      var booleans: Boolean = true
      var loops: Boolean = false
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
    hoist_vars = false
    hoist_funs = false
    booleans = false
    unsafe_comps = false
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

  implicit class AST_NodeOps(val node: AST_Node) {
    def walk(walker: AST_Node => Boolean): Unit = node.walk_js(new TreeWalker((node, _) => walker(node)))

    def transform(transformer: (AST_Node, (AST_Node, TreeTransformer) => AST_Node, TreeTransformer) => AST_Node): AST_Node = {
      var tr: TreeTransformer = null
      tr = new TreeTransformer((node, descend) => transformer(node, descend, tr))
      node.transform_js(tr)
    }

  }

  def nodeClassName(n: AST_Node): String = {
    if (js.isUndefined(n)) "undefined"
    else {
      val nd = n.asInstanceOf[js.Dynamic]
      val s = nd.constructor.name.asInstanceOf[String]
      if (s == "AST_Node" && nd.CTOR != null) {
        nd.CTOR.name.asInstanceOf[String]
      } else s
    }
  }


  def uglify(code: String, options: Options = defaultUglifyOptions): String = {

    val toplevel_ast = parse(code, options.parse)

    val compressed_ast = toplevel_ast.optimize(options.compress)

    // 3. Mangle
    compressed_ast.mangleNames()

    // 4. Generate output
    val outCode = compressed_ast.source(options.output)

    outCode
  }


}
