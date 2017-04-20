package com.github.opengrabeso

import scala.language.implicitConversions
import scala.scalajs.js
import scala.scalajs.js.RegExp
import scala.scalajs.js.annotation._
import JsUtils._
import js.JSConverters._

object Helpers {
  @js.native
  trait CloneSelf[+T <: js.Object] extends js.Object {
    self: T =>
    override def clone(): T = js.native
  }
}

@JSGlobal("UglifyJS")
@js.native
object Uglify extends js.Object {
  import Helpers._

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
    // points to the AST_Scope where this is defined. Beware: for var this is a "hoisted" scope
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

    var index: js.Any = js.native
    var id: js.Any = js.native
  }



  // f:  return true to abort the walk
  @js.native class TreeWalker(f: js.Function2[AST_Node, js.Function2[AST_Node, TreeWalker, Unit], Boolean]) extends js.Any {
    // returns the parent of the current node.
    def parent(n: Int = 0): js.UndefOr[AST_Node] = js.native
    // an array holding all nodes that lead to current node. The last element in this array is the current node itself.
    def stack: js.Array[AST_Node] = js.native
    // finds the innermost parent of the given type. type must be a node constructor, i.e. AST_Scope.
    val find_parent: (js.Dynamic) => AST_Node = js.native
  }

  @js.native class TreeTransformer(
    before: js.Function2[AST_Node, js.Function2[AST_Node, TreeTransformer, AST_Node], AST_Node],
    after: js.Function1[AST_Node, AST_Node] = js.native
  ) extends TreeWalker(js.native)

  @js.native sealed abstract class AST_Node extends js.Object with CloneSelf[AST_Node] {
    var start: js.UndefOr[AST_Token] = js.native
    var end: js.UndefOr[AST_Token] = js.native

    @JSName("walk")
    def walk_js(walker: TreeWalker): Unit = js.native

    @JSName("transform")
    def transform_js(transformer: TreeTransformer): AST_Node = js.native
  }

  @js.native sealed abstract class AST_Statement extends AST_Node

  @js.native class AST_Debugger extends AST_Statement

  @js.native class AST_Directive extends AST_Statement

  @js.native class AST_SimpleStatement extends AST_Statement {
    var body: AST_Node = js.native // [AST_Node] an expression node (should not be instanceof AST_Statement)
  }

  @js.native class AST_Block extends AST_Statement {
    @JSName("body")
    var _body: js.Any = js.native
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
    val enclosed: js.UndefOr[js.Array[SymbolDef]] = js.native
    // [integer/S] current index for mangling variables (used internally by the mangler)
    val cname: js.UndefOr[Int] = js.native
  }

  @js.native class AST_Toplevel extends AST_Scope with CloneSelf[AST_Toplevel] {
    def figure_out_scope(): Unit = js.native

    def transform(c: Compressor): AST_Toplevel = js.native

    def compute_char_frequency(): Unit = js.native

    def mangle_names(): Unit = js.native

    def print_to_string(config: UglifyExt.Options.Output): String = js.native
  }

  @js.native class AST_Lambda extends AST_Scope with CloneSelf[AST_Lambda] {
    var name: js.UndefOr[AST_SymbolDeclaration] = js.native
    //[AST_SymbolDeclaration?] the name of this function
    var argnames: js.Array[AST_SymbolFunarg] = js.native
    // [AST_SymbolFunarg*] array of function arguments
    var uses_arguments: js.UndefOr[Boolean] = js.native // "[boolean/S] tells whether this function accesses the arguments array"
  }

  @js.native class AST_Accessor extends AST_Lambda

  @js.native class AST_Function extends AST_Lambda

  @js.native class AST_Arrow extends AST_Lambda

  @js.native class AST_Defun extends AST_Lambda

  @js.native class AST_Switch extends AST_Scope {
    val expression: AST_Node = js.native // [AST_Node] the `switch` “discriminant
  }

  @js.native sealed abstract class AST_SwitchBranch extends AST_Scope

  @js.native class AST_Default extends AST_SwitchBranch

  @js.native class AST_Case extends AST_SwitchBranch {
    // [AST_Node] the `case` expression
    var expression: AST_Node = js.native
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
    var body: AST_Statement = js.native
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
    var condition: AST_Node  = js.native
    // [AST_Statement?] the `else` part, or null if not present
    var alternative: js.UndefOr[AST_Statement]  = js.native
  }

  @js.native sealed abstract class AST_Jump extends AST_Statement

  @js.native sealed abstract class AST_Exit extends AST_Jump {
    // [AST_Node?] the value returned or thrown by this statement; could be null for AST_Return
    var value: js.UndefOr[AST_Node]  = js.native
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
    var definitions: js.Array[AST_VarDef] = js.native
  }

  @js.native class AST_Var extends AST_Definitions with CloneSelf[AST_Var]
  @js.native class AST_Const extends AST_Definitions with CloneSelf[AST_Const]
  @js.native class AST_Let extends AST_Definitions with CloneSelf[AST_Let]

  @js.native class AST_VarDef extends AST_Node with CloneSelf[AST_VarDef] {
    // [AST_SymbolVar|AST_SymbolConst] name of the variable
    var name: AST_SymbolVarOrConst = js.native
    // "[AST_Node?] initializer, or null of there's no initializer"
    var value: js.UndefOr[AST_Node] = js.native
  }

  @js.native class AST_Call extends AST_Node {
    //[AST_Node] expression to invoke as function
    var expression: AST_Node = js.native
    //[AST_Node*] array of arguments
    var args: js.Array[AST_Node] = js.native
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
    var expression: AST_Node = js.native
    // [AST_Node|string] the property to access.  For AST_Dot this is always a plain string, while for AST_Sub it's an arbitrary AST_Node
    def property: Any = js.native
    def property_=(p: js.Any): Unit = js.native
  }

  @js.native class AST_Dot extends AST_PropAccess {
    override def property: String = js.native
  }
  @js.native class AST_Sub extends AST_PropAccess {
    override val property: AST_Node = js.native
  }

  @js.native sealed abstract class AST_Unary extends AST_Node {
    // [string] the operator
    var operator: String = js.native
    // [AST_Node] expression that this unary operator applies to
    var expression: AST_Node = js.native
  }

  @js.native class AST_UnaryPrefix extends AST_Unary
  @js.native class AST_UnaryPostfix extends AST_Unary

  @js.native class AST_Binary extends AST_Node {
    // [AST_Node] left-hand side expression
    var left: AST_Node = js.native
    // [string] the operator
    var operator: String = js.native
    // [AST_Node] right-hand side expression
    var right: AST_Node = js.native
  }

  @js.native class AST_Assign extends AST_Binary

  @js.native class AST_Conditional extends AST_Node {
    // [AST_Node]
    var condition: AST_Node = js.native
    // [AST_Node]
    var consequent: AST_Node = js.native
    // [AST_Node]
    var alternative: AST_Node = js.native
  }

  @js.native class AST_Array extends AST_Node {
    // [AST_Node*] array of elements
    var elements: js.Array[AST_Node] = js.native
  }

  @js.native class AST_Object extends AST_Node with CloneSelf[AST_Object] {
    // [AST_ObjectProperty*] array of properties
    var properties: js.Array[AST_ObjectProperty] = js.native
  }

  @js.native sealed abstract class AST_ObjectProperty extends AST_Node {
    // [string] the property name converted to a string for ObjectKeyVal.  For setters and getters this is an arbitrary AST_Node.
    def key: Any = js.native
    def key_=(k: Any): Unit = js.native

    // [AST_Node] property value.  For setters and getters this is an AST_Function.
    def value: AST_Node = js.native
    def value_= (node: AST_Node): Unit = js.native
  }

  @js.native class AST_ObjectKeyVal extends AST_ObjectProperty {
    override def key: String = js.native

    // [string] the original quote character
    var quote: String =  js.native
  }

  // beware: type does not exist in Uglify.js, do not match against it!
  @js.native sealed abstract class AST_ObjectSetterOrGetter extends AST_ObjectProperty {
    // [string] the property name converted to a string for ObjectKeyVal.  For setters and getters this is an arbitrary AST_Node.
    override def key: AST_Symbol = js.native
    // [AST_Node] property value.  For setters and getters this is an AST_Function.
    override def value: AST_Function = js.native
    // [boolean] whether this method is static (classes only)
    var `static`: Boolean = js.native
  }
  @js.native class AST_ObjectSetter extends AST_ObjectSetterOrGetter
  @js.native class AST_ObjectGetter extends AST_ObjectSetterOrGetter

  @js.native class AST_ConciseMethod extends AST_ObjectProperty with CloneSelf[AST_ConciseMethod] {
    override def key: AST_Symbol = js.native
    override def value: AST_Accessor = js.native

    // [string|undefined] the original quote character, if any
    var quote: js.UndefOr[String]= js.native
    // [boolean] whether this method is static (classes only)
    var `static`: Boolean = js.native
    // [boolean] is generatorFn or not
    var is_generator: Boolean = js.native
  }


  @js.native sealed class AST_Symbol extends AST_Node with CloneSelf[AST_Symbol] {
    // [string] name of this symbol
    var name: String = js.native
    // [AST_Scope/S] the current scope (not necessarily the definition scope)
    var scope: js.UndefOr[AST_Scope] = js.native
    // [SymbolDef/S] the definition of this symbol
    var thedef: js.UndefOr[SymbolDef] = js.native
  }

  @js.native class AST_SymbolAccessor extends AST_Symbol
  @js.native class AST_SymbolMethod extends AST_Symbol
  @js.native class AST_SymbolDeclaration extends AST_Symbol with CloneSelf[AST_SymbolDeclaration] {
    // [AST_Node*/S] array of initializers for this declaration.
    var init: js.UndefOr[js.Array[AST_Node]] = js.native
  }

  @js.native class AST_SymbolVarOrConst extends AST_SymbolDeclaration
  @js.native class AST_SymbolVar extends AST_SymbolVarOrConst
  @js.native class AST_SymbolFunarg extends AST_SymbolVar with CloneSelf[AST_SymbolFunarg]
  @js.native class AST_SymbolConst extends AST_SymbolVarOrConst

  @js.native class AST_SymbolDefun extends AST_SymbolDeclaration
  @js.native class AST_SymbolLambda extends AST_SymbolDeclaration
  @js.native class AST_SymbolCatch extends AST_SymbolDeclaration
  @js.native class AST_SymbolDefClass extends AST_SymbolDeclaration
  @js.native class AST_SymbolImport extends AST_SymbolDeclaration
  @js.native class AST_SymbolImportForeign extends AST_SymbolDeclaration
  @js.native class AST_NameImport extends AST_Node {
    var foreign_name: AST_SymbolImportForeign = js.native // The name being imported (as specified in the module)
    var name: AST_SymbolImport = js.native // The name as it becomes available to this module
  }

  @js.native class AST_Label extends AST_Symbol {
    // [AST_LoopControl*] a list of nodes referring to this label
    val references: js.Array[AST_LoopControl] = js.native
  }

  @js.native class AST_SymbolRef extends AST_Symbol with CloneSelf[AST_SymbolRef]
  @js.native class AST_LabelRef extends AST_Symbol
  @js.native class AST_This extends AST_Symbol
  @js.native class AST_Super extends AST_Symbol

  @js.native sealed abstract class AST_Constant extends AST_Node

  @js.native class AST_String extends AST_Constant {
    // [string] the contents of this string
    var value: String = js.native
    // [string] the original quote character
    var quote: String  = js.native
  }
  @js.native class AST_Number extends AST_Constant {
    // [number] the numeric value
    var value: Double = js.native
    // [string] numeric value as string (optional)
    var literal: js.UndefOr[String] = js.native
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

  @js.native class AST_Class extends AST_Scope with CloneSelf[AST_Class] {
    // [AST_SymbolClass|AST_SymbolDefClass?] optional class name.
    var name: js.UndefOr[AST_Symbol] = js.native
    // [AST_Node]? optional parent class
    var `extends`: js.UndefOr[AST_Node] = js.native
    // "[AST_ObjectProperty*] array of properties"
    var properties: js.Array[AST_ObjectProperty] = js.native
  }

  @js.native class AST_DefClass extends AST_Class with CloneSelf[AST_DefClass]

  @js.native class AST_Export extends AST_Node {
    // String literal describing where this module came from
    var module_name: js.UndefOr[AST_String] = js.native
    // An exported value
    var exported_value: js.UndefOr[AST_Node] = js.native
    // An exported definition
    var exported_definition: js.UndefOr[AST_Node] = js.native
  }

  @js.native class AST_Import extends AST_Node {
    //  The name of the variable holding the module's default export
    var imported_name: js.UndefOr[AST_SymbolImport] = js.native
    //  The names of non-default imported variables
    var imported_names: js.UndefOr[js.Array[AST_NameImport]] = js.native
    // String literal describing where this module came from
    var module_name: AST_String = js.native
  }

  @js.native
  class Compressor(options: UglifyExt.Options.Compress) extends js.Object

  def parse(code: String, options: UglifyExt.Options.Parse): AST_Toplevel = js.native

  @js.native
  class JS_Parse_Error extends js.Object {
    val message: String = js.native
    val filename: String = js.native
    val line: Int = js.native
    val col: Int = js.native
    val pos: Int = js.native
  }
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

  implicit class AST_BlockOps(val block: AST_Block) {
    // workaround for issue https://github.com/mishoo/UglifyJS2/issues/1499
    def body: js.Array[AST_Statement] = block._body match {
      case ba: js.Array[AST_Statement@unchecked] => ba
      case bn: AST_Statement => js.Array(bn)
      case x if js.isUndefined(x) => js.Array()
      case x =>
        println(s"Unexpected block body $x in ${nodeClassName(block)}")
        js.Array()
    }

    def body_=(b: js.Array[AST_Statement]) = block._body = b
  }

  implicit class AST_NodeOps[T <: AST_Node](val node: T) {

    def walk(walker: AST_Node => Boolean): Unit = node.walk_js(new TreeWalker((node, _) => walker(node)))
    def walkWithDescend(walker: (AST_Node, (AST_Node, TreeWalker) => Unit, TreeWalker) => Boolean): Unit = {

      var w: TreeWalker = null
      w = new TreeWalker((node, descend) => walker(node, descend, w))
      node.walk_js(w)
    }

    def transformBefore(before: (AST_Node, (AST_Node, TreeTransformer) => AST_Node, TreeTransformer) => AST_Node): T = {
      var tr: TreeTransformer = null
      tr = new TreeTransformer((node, descend) => before(node, descend, tr))
      node.transform_js(tr).asInstanceOf[T]
    }

    def transformAfter(after: (AST_Node, TreeTransformer) => AST_Node): T = {
      var tr: TreeTransformer = null
      tr = new TreeTransformer(null, node => after(node, tr))
      node.transform_js(tr).asInstanceOf[T]
    }

    def withTokens(from: AST_Node): T = {
      fillTokens(node, from)
      node
    }

  }

  trait AST_Extractors {
    def init[T](t: T)(i: T => Unit): T = {
      i(t)
      t
    }

    object AST_Binary {
      def unapply(arg: AST_Binary) = Some((arg.left, arg.operator, arg.right))
    }
    object AST_Assign {
      def unapply(arg: AST_Assign) = AST_Binary.unapply(arg)
    }

    object AST_Symbol {
      def unapply(arg: AST_Symbol) = Some((arg.name, arg.scope, arg.thedef))
    }
    object AST_SymbolName {
      def unapply(arg: AST_Symbol) = Some(arg.name)
    }

    object AST_SymbolRef {
      def unapply(arg: AST_SymbolRef) = AST_Symbol.unapply(arg)

      def apply(from: AST_Node)(n: String): AST_SymbolRef = {
        init(new AST_SymbolRef){ s =>
          s.name = n
        }.withTokens(from)
      }
      def symDef(from: AST_Node)(sd: SymbolDef): AST_SymbolRef = {
        init(new AST_SymbolRef){ s =>
          s.name = sd.name
          s.thedef = sd
        }.withTokens(from)
      }
      def sym(from: AST_Node)(sym: AST_Symbol): AST_SymbolRef = {
        init(new AST_SymbolRef){ s =>
          s.name = sym.name
          s.thedef = sym.thedef
          s.scope = sym.scope
        }.withTokens(from)
      }
    }
    object AST_SymbolRefName {
      def unapply(arg: AST_SymbolRef) = AST_SymbolName.unapply(arg)
    }

    object AST_SymbolRefUndefined {
      def unapply(arg: AST_SymbolRef) = arg match {
        case AST_SymbolRefName("undefined") => true
        case _ => false
      }
    }


    object AST_EmptyStatement {
      def apply(from: AST_Node): AST_EmptyStatement = new AST_EmptyStatement().withTokens(from)
    }

    object AST_SimpleStatement {
      def unapply(arg: AST_SimpleStatement) = Some(arg.body)

      def apply(from: AST_Node)(body: AST_Node): AST_SimpleStatement = init(new AST_SimpleStatement()) { node =>
        node.body = body
      }.withTokens(from)
    }

    object AST_BlockStatement {
      def unapply(arg: AST_BlockStatement) = Some(arg.body.toSeq)
    }

    object AST_VarDef {
      def unapply(arg: AST_VarDef) = Some(arg.name, arg.value)

      def apply(from: AST_Node)(name: AST_SymbolVarOrConst, value: js.UndefOr[AST_Node]): AST_VarDef = {
        init(new AST_VarDef()) { node =>
          node.name = name
          node.value = value
        }
      }

      def uninitialized(node: AST_Node)(vName: String): AST_VarDef = {
        AST_VarDef(node)(
          new AST_SymbolVar {
            fillTokens(this, node)
            name = vName
            // thedef and scope will be filled by uglify
            init = js.Array[AST_Node]()
          },
          js.undefined
        )
      }

      def initialized(node: AST_Node)(vName: String, right: AST_Node): AST_VarDef = {
        AST_VarDef(node)(
          new AST_SymbolVar {
            fillTokens(this, node)
            name = vName
            // thedef and scope will be filled by uglify
            init = js.Array(right)
          },
          right
        )

      }
    }

    object AST_Unary {
      def unapply(arg: AST_Unary) = Some(arg.operator, arg.expression)
    }
    object AST_UnaryPrefix {
      def unapply(arg: AST_UnaryPrefix) = AST_Unary.unapply(arg)
    }

    object AST_UnaryPostfix {
      def unapply(arg: AST_UnaryPostfix) = AST_Unary.unapply(arg)
    }

    object AST_Definitions {
      def unapplySeq(arg: AST_Definitions) = Some(arg.definitions.toSeq)
    }

    object AST_Let {
      def unapplySeq(arg: AST_Let) = AST_Definitions.unapplySeq(arg)
    }

    object AST_Var {
      def unapplySeq(arg: AST_Var) = AST_Definitions.unapplySeq(arg)
    }

    object AST_Const {
      def unapplySeq(arg: AST_Const) = AST_Definitions.unapplySeq(arg)
    }

    object AST_String {
      def unapply(arg: AST_String) = Some(arg.value)
    }

    object AST_This {
      def unapply(arg: AST_This): Boolean = true
    }
    object AST_SymbolDeclaration {
      def unapply(arg: AST_SymbolDeclaration) = Some(arg.thedef, arg.name, arg.init)

    }

    object AST_SymbolFunarg {
      def unapply(arg: AST_SymbolFunarg) = AST_SymbolDeclaration.unapply(arg)
    }

    object AST_Number {
      def unapply(arg: AST_Number) = Some(arg.value)
    }

    object AST_Defun {
      def unapply(arg: AST_Defun) = Some(arg.name, arg.argnames.toSeq, arg.body.toSeq)
    }

    object AST_Lambda {
      def unapply(arg: AST_Lambda) = Some(arg.argnames.toSeq, arg.body.toSeq)
    }

    object AST_Function {
      def unapply(arg: AST_Function) = AST_Lambda.unapply(arg)
    }

    object AST_Accessor {
      def unapply(arg: AST_Accessor) = AST_Lambda.unapply(arg)
    }

    object AST_Return {
      def unapply(arg: AST_Return) = Some(arg.value)
    }

    object AST_Dot {
      def unapply(arg: AST_Dot) = Some(arg.expression, arg.property)
    }

    object AST_Call {
      def unapplySeq(arg: AST_Call): Option[(AST_Node, Seq[AST_Node])] = Some(arg.expression, arg.args)
    }

    object AST_New {
      def unapplySeq(arg: AST_New) = AST_Call.unapplySeq(arg)
    }

    object AST_Conditional {
      def unapply(arg: AST_Conditional) = Some(arg.condition, arg.consequent, arg.alternative)
    }

    object AST_If {
      def unapply(arg: AST_If) = Some(arg.condition, arg.body, arg.alternative.nonNull)
    }

    object AST_Switch {
      def unapply(arg: AST_Switch) = Some(arg.expression, arg._body.asInstanceOf[js.Array[AST_SwitchBranch]].toSeq)
    }

    object AST_DefClass {
      def unapply(arg: AST_DefClass) = Some(arg.name, arg.`extends`, arg.properties)
    }

    object AST_Sub {
      def unapply(arg: AST_Sub) = Some(arg.expression, arg.property)
    }

    object AST_Object {
      def unapply(arg: AST_Object) = Some(arg.properties.toSeq)
    }
    object AST_ConciseMethod {
      def unapply(arg: AST_ConciseMethod) = Some(arg.key, arg.value)
    }

    object AST_ObjectGetter {
      def unapply(arg: AST_ObjectGetter) = Some(arg.key, arg.value)
    }
    object AST_ObjectSetter {
      def unapply(arg: AST_ObjectSetter) = Some(arg.key, arg.value)
    }

    object AST_ObjectKeyVal {
      def unapply(arg: AST_ObjectKeyVal) = Some(arg.key, arg.value)
    }


    // helpers, composite extractors
    object Defined {
      def unapply[T](arg: js.UndefOr[T])(implicit ev: Null <:< T): Option[T] = arg.nonNull
    }

    object JsArray {
      def unapplySeq[T](arg: js.Array[T]): Some[Seq[T]] = Some(arg.toSeq)
    }

    object AST_SymbolDef {
      def unapply(arg: AST_Node): Option[SymbolDef] = arg match {
        case AST_Symbol(_, _, Defined(sym)) => Some(sym)
        case _ => None
      }
    }

    object AST_SymbolRefDef {
      def unapply(arg: AST_Node): Option[SymbolDef] = arg match {
        case AST_SymbolRef(_, _, Defined(sym)) => Some(sym)
        case _ => None
      }
    }

    object VarName {
      def unapply(arg: AST_Node) = arg match {
        case AST_Definitions(AST_VarDef(AST_SymbolName(s), _)) => Some(s)
        case _ => None
      }
    }

    object UnaryModification {
      def unapply(arg: String): Boolean = arg == "++" || arg == "--"
    }

    object Statements {
      def unapply(arg: AST_Node) = arg match {
        case AST_BlockStatement(body) => Some(body)
        case s@AST_SimpleStatement(body) => Some(Seq(s))
        case _ => None
      }
    }

    object SingleStatement {
      def unapply(arg: AST_Node) = arg match {
        case AST_BlockStatement(Seq(AST_SimpleStatement(body))) => Some(body)
        case AST_SimpleStatement(body) => Some(body)
        case _ => None
      }
    }

  }

  object Import extends AST_Extractors
  import Import._

  def nodeClassName(n: AST_Node): String = {
    if (js.isUndefined(n)) "undefined"
    else if (n == null) "null"
    else {
      val nd = n.asInstanceOf[js.Dynamic]
      val s = nd.constructor.name.asInstanceOf[String]
      if (s == "AST_Node" && nd.CTOR != null) {
        nd.CTOR.name.asInstanceOf[String]
      } else s
    }
  }

  def scopeName(n: AST_Scope): String = {
    nodeClassName(n) + ":" + n.start.fold("")(pos => pos.line.toString)
  }

  def nodeTreeToString(t: AST_Node): String = {
    val sb = new StringBuilder
    new AST_NodeOps(t).walkWithDescend { (n, descend, walker) =>
      sb.append("{\n")
      sb.append(nodeClassName(n))
      sb.append(":")
      descend(n, walker)
      sb.append("}\n")
      true
    }
    sb.result()
  }

  def fillTokens(to: AST_Node, from: AST_Node): Unit = {
    to.start = from.start
    to.end = from.end
  }

  def keyNode(orig: AST_Node, k: String) = AST_SymbolRef(orig)(k)


  def newMethod(k: String, args: Seq[AST_SymbolFunarg], body: Seq[AST_Statement], tokensFrom: AST_Node, isStatic: Boolean = false) = new AST_ConciseMethod {
    fillTokens(this, tokensFrom)
    key = keyNode(tokensFrom, k)
    `static` = isStatic
    value = new AST_Accessor {
      name = new AST_SymbolDefun {
        /*_*/
        fillTokens(this, tokensFrom)
        /*_*/
        name = k
      }
      fillTokens(this, tokensFrom)
      argnames = args.toJSArray
      this.body = body.toJSArray

    }
  }


  def unsupported(message: String, source: AST_Node, include: Option[AST_Node] = None) = {
    AST_SimpleStatement(source) {
      new AST_Call {
        fillTokens(this, source)
        expression = AST_SymbolRef(source)("????") // force compile error
        args = js.Array(
          new AST_String {
            fillTokens(this, source)
            value = message
            quote = "'"
          }
        ) ++ include.map(AST_SimpleStatement(source))
      }
    }
  }

  def propertyName(prop: AST_ObjectProperty): String = prop match {
    case p: AST_ObjectKeyVal => p.key
    case p: AST_ObjectSetter => p.key.name
    case p: AST_ObjectGetter => p.key.name
    case p: AST_ConciseMethod => p.key.name
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
