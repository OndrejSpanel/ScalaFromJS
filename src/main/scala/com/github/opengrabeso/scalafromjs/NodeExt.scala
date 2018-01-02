package com.github.opengrabeso.scalafromjs

import com.github.opengrabeso.esprima.Node
import com.github.opengrabeso.esprima.Node.BlockStatement
import com.github.opengrabeso.scalafromjs.esprima.symbols.ScopeContext
import esprima._

import scala.reflect.ClassTag

trait NodeExt {

  implicit class NodeOps[T <: Node.Node](n: T) {
    def start: Option[Int] = Option(n.range).map(_._1)
    def end: Option[Int] = Option(n.range).map(_._2)

    def copyLoc(from: Node.Node): T = {
      n.copyNode(from)
      n
    }

    def withTokens(from: Node.Node): T = {
      assert(from.range != null)
      copyLoc(from)
    }

    def cloneNode(): T = n.clone().asInstanceOf[T]

    def withTokensDeep(from: Node.Node): T = {
      n.walk { node =>
        node.copyNode(from)
        false
      }
      n
    }
  }

  def nodeClassName(node: Node.Node): String = node.getClass.getSimpleName


  // short aliases (often inspired by Uglify AST names)
  type Dot = Node.StaticMemberExpression

  object Dot {
    def unapply(arg: Node.StaticMemberExpression) = arg.property match{
      case Node.Identifier(name) =>
        Some(arg.`object`, name)
      case _ =>
        None
    }
    def apply(`object`: Node.Expression, property: Node.Expression) = Node.StaticMemberExpression.apply(`object`, property)
  }

  type Sub = Node.ComputedMemberExpression
  val Sub = Node.ComputedMemberExpression

  type Conditional = Node.ConditionalExpression
  val Conditional = Node.ConditionalExpression

  // not named Array, so that it is not shadowing the default Scala name
  type AArray = Node.ArrayExpression
  val AArray = Node.ArrayExpression

  type OObject = Node.ObjectExpression
  val OObject = Node.ObjectExpression

  type VarDef = Node.VariableDeclarator
  val VarDef = Node.VariableDeclarator

  type DefFun = Node.FunctionDeclaration
  val DefFun = Node.FunctionDeclaration

  object Binary {
    def unapply(arg: Node.BinaryExpression): Option[(Node.Expression, String, Node.Expression)] = Some(arg.left, arg.operator, arg.right)
    def apply(left: Node.Expression, op: String, right: Node.Expression): Node.BinaryExpression = Node.BinaryExpression(op, left, right)
  }

  object Assign {
    def unapply(arg: Node.Node): Option[(Node.Expression, String, Node.Expression)] = arg match {
      case arg: Node.AssignmentExpression =>
        Some(arg.left, arg.operator, arg.right)
      case Node.AssignmentPattern(left: Node.Expression, right) =>
        Some(left, "=", right)
      case _ =>
        None
    }
    def apply(left: Node.Expression, op: String, right: Node.Expression): Node.AssignmentExpression = Node.AssignmentExpression(op, left, right)
  }

  object ObjectKeyVal {
    def unapply(arg: Node.Property) = {
      val key: String = arg.key match {
        case Node.Identifier(name) =>
          name
        case Node.Literal(value, raw) =>
          value
      }
      Some(key, arg.value)
    }
  }

  object Defined {
    // extract value from a potential null only if non-null
    def unapply[T](some: T): Option[T] = {
      Option(some)
    }
  }

  object IsNull {
    // extract value from a potential null only if non-null
    def unapply[T](some: T): Boolean = {
      some == null
    }
  }

  object MayBeNull {
    // extract value from a potential null as an option
    def unapply[T](some: T): Option[Option[T]] = {
      Some(Option(some))
    }
  }

  class LiteralExtractor[T: ClassTag] {
    def unapply(arg: Node.Literal): Option[T] = {
      if (arg.value == null) None
      else {
        arg.value.value match {
          case s: T => Some(s)
          case _ => None
        }
      }
    }
  }

  object StringLiteral extends LiteralExtractor[String]
  object NumberLiteral extends LiteralExtractor[Double]
  object BooleanLiteral extends LiteralExtractor[Boolean]

  object Block {
    def apply(node: Node.BlockStatementOrExpression): Node.BlockStatement = node match {
      case node: Node.BlockStatement =>
        node
      case expr: Node.Expression =>
        Node.BlockStatement(Seq(Node.ExpressionStatement(expr))).withTokens(expr)

    }

    def statements(s: Node.Node): Seq[Node.Statement] = {
      s match {
        case b: Node.BlockStatement =>
          b.body.asInstanceOf[Seq[Node.Statement]]
        case s: Node.Statement =>
          Seq(s)
        case s: Node.Expression =>
          Seq(Node.ExpressionStatement(s))
      }
    }
  }


  object AnyFunctionExpression {
    def unapply(arg: Node.Node): Option[(Seq[Node.FunctionParameter], Node.BlockStatementOrExpression)] = arg match {
      case f: Node.FunctionExpression =>
        Some(f.params, f.body)
      case f: Node.ArrowFunctionExpression =>
        Some(f.params, f.body)
      case _ =>
        None
    }
  }

  object AnyFun {
    def unapply(arg: Node.Node): Option[(Seq[Node.FunctionParameter], Node.BlockStatementOrExpression)] = arg match {
      case f: Node.FunctionExpression =>
        Some(f.params, f.body)
      case f: Node.ArrowFunctionExpression =>
        Some(f.params, f.body)
      case f: Node.FunctionDeclaration =>
        Some(f.params, f.body)
      case f: Node.AsyncFunctionExpression =>
        Some(f.params, f.body)
      case f: Node.AsyncFunctionDeclaration =>
        Some(f.params, f.body)
      case f: Node.AsyncArrowFunctionExpression =>
        Some(f.params, f.body)
      case _ =>
        None
    }
  }


  object IsFunctionScope {
    def unapply(arg: Node.Node): Boolean = arg match {
      case AnyFun(_, _) =>
        true
      case _ =>
        false
    }
  }

  object AnyLoop {
    def unapply(arg: Node.Node): Option[(Node.Expression, Node.Statement)] = arg match {
      case f: Node.ForStatement =>
        Some(f.test, f.body)
      case f: Node.ForInStatement =>
        Some(f.right, f.body)
      case f: Node.ForOfStatement =>
        Some(f.right, f.body)
      case f: Node.WhileStatement =>
        Some(f.test, f.body)
      case f: Node.DoWhileStatement =>
        Some(f.test, f.body)
      case _ =>
        None

    }
  }

  object IsStatementScope {
    def unapply(arg: Node.Node): Boolean = arg match {
      case AnyLoop(_, _) =>
        true
      case _ =>
        false
    }
  }

  object IsUnconditionalScope {
    def unapply(arg: Node.Node): Boolean = arg match {
      case _: Node.ClassBody =>
        true
      case _: Node.Program =>
        true
      case IsFunctionScope() =>
        true
      case IsStatementScope() =>
        true
      /*
      case _: Node.IfStatement =>
        // note: each branch statement could be considered a saperate scope, however cases needing this should be very rare
        // ... and such cases should (and most likely will) use BlockStatement as a branch body anyway
        true
      */
      case _ =>
        false

    }

  }
  object IsDeclScope {
    def unapply(arg: Node.Node)(implicit context: ScopeContext): Boolean = arg match {
      case block: Node.BlockStatement =>
        context.parent() match {
          case Some(AnyFun(_, `block`)) =>
            // body directly inside of a function is not considered a scope
            false
          case Some(AnyLoop(_, `block`)) =>
            // body directly inside of a control statement is not considered a scope, as it shares the scope
            false
          case _ =>
            true
        }
      case IsUnconditionalScope() =>
        true
      case _ =>
        false
    }
  }

  // simplified variant, conservative, does not require implicit ScopeContext
  object IsScope {
    def unapply(arg: Node.Node): Boolean = arg match {
      case _: Node.BlockStatement =>
        true
      case IsUnconditionalScope() =>
        true
      case _ =>
        false
    }
  }

  object VarDecl {
    def unapply(arg: Node.VariableDeclaration): Option[(String, Option[Node.Expression], String)] = arg match {
      case Node.VariableDeclaration(Seq(Node.VariableDeclarator(Node.Identifier(name), MayBeNull(init))), kind) =>
        Some(name, init, kind)
      case _ =>
        None
    }

    def apply(name: String, init: Option[Node.Expression], kind: String): Node.VariableDeclaration = {
      Node.VariableDeclaration(Seq(Node.VariableDeclarator(Node.Identifier(name), init.orNull)), kind)
    }
  }

  def parameterName(n: Node.FunctionParameter): (Node.Identifier, Option[Node.Node]) = {
    (n: @unchecked) match {
      case Node.AssignmentPattern(left: Node.Identifier, right) =>
        left -> Some(right)
      case id: Node.Identifier =>
        id -> None
      //case _: Node.ArrowParameterPlaceHolder =>
      //case _: Node.BindingPattern =>
    }
  }

  def parameterNameString(n: Node.FunctionParameter): String = {
    parameterName(n)._1.name
  }

  object KeyName {
    def unapply(arg: Node.PropertyKey) = {
      Some(propertyKeyName(arg))
    }
  }

  def propertyKeyName(pk: Node.PropertyKey): String = {
    pk match {
      case Node.Identifier(name) =>
        name
      case Node.Literal(value, raw) =>
        value
    }
  }
  def propertyName(n: Node.ObjectExpressionProperty): String = {
    (n: @unchecked) match {
      case p: Node.Property =>
        p.key match {
          case Node.Identifier(name) =>
            name
          case Node.Literal(value, raw) =>
            value
        }
    }
  }

  def methodName(n: Node.ClassBodyElement): String = {
    n match {
      case n: Node.MethodDefinition =>
        n.key match {
          case Node.Identifier(name) =>
            name
        }
    }
  }

  def unsupported(message: String, source: Node.Node, include: Option[Node.Node] = None) = {
    Node.ExpressionStatement {
      Node.CallExpression(
        Node.Identifier("????"), // force compile error
        Seq(Node.Literal(message, message)) // ++ include.map(i => Node.ExpressionStatement.apply(i))
      )
    }.withTokensDeep(source)
  }



  def propertyValue(n: Node.ObjectExpressionProperty): Option[Node.Node] = {
    n match {
      case p: Node.Property =>
        Some(p.value)
      case _ =>
        None
    }
  }

  def keyValIsTemplate(kv: Node.ObjectExpressionProperty): Boolean = propertyName(kv) startsWith Symbols.templatePrefix

  object ExportFromSource {
    def unapply(arg: Node.ExportDeclaration) = arg match {
      case Node.ExportAllDeclaration(source) =>
        Some(source)
      case Node.ExportNamedDeclaration(declaration, specifiers, source) =>
        Some(source)
      case _ =>
        None

    }
  }

  object NodeExtended {
    def noTypes = SymbolTypes()
  }
  case class NodeExtended(top: Node.Program, types: SymbolTypes = SymbolTypes(), config: ConvertProject.ConvertConfig = ConvertProject.ConvertConfig()) {
    def loadConfig: NodeExtended = {
      val (config,ast) = ConvertProject.loadConfig(top)

      copy(top = ast, config = config)
    }

  }


}
