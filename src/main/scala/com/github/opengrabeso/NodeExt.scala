package com.github.opengrabeso

import _root_.esprima.Node
import esprima._

trait NodeExt {

  implicit class NodeOps[T <: Node.Node](n: T) {
    def start: Option[Int] = Option(n.range).map(_._1)
    def end: Option[Int] = Option(n.range).map(_._2)

    def copyLoc(from: Node.Node): T = {
      n.copyNode(from)
      n
    }

    def withTokens(from: Node.Node): T = copyLoc(from)

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

  object StringLiteral {
    def unapply(arg: Node.Literal): Option[String] = arg.value.value match {
      case s: String => Some(s)
      case _ => None

    }
  }
  object NumberLiteral {
    def unapply(arg: Node.Literal): Option[Double] = arg.value.value match {
      case s: Double => Some(s)
      case _ => None
    }
  }
  object BooleanLiteral {
    def unapply(arg: Node.Literal): Option[Boolean] = arg.value.value match {
      case s: Boolean => Some(s)
      case _ => None
    }
  }

  object IsFunctionScope {
    def unapply(arg: Node.Node): Boolean = arg match {
      case _: Node.FunctionExpression =>
        true
      case _: Node.ArrowFunctionExpression =>
        true
      case _: Node.FunctionDeclaration =>
        true
      case _: Node.AsyncFunctionExpression =>
        true
      case _: Node.AsyncFunctionDeclaration =>
        true
      case _: Node.AsyncArrowFunctionExpression =>
        true
      case _ =>
        false
    }
  }
  object IsScope {
    def unapply(arg: Node.Node): Boolean = arg match {
      case _: Node.BlockStatement =>
        true
      case _: Node.ClassBody =>
        true
      case _: Node.Program =>
        true
      case IsFunctionScope() =>
        true
      case _ =>
        false
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
