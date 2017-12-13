package com.github.opengrabeso

import _root_.esprima.Node

trait NodeExt {

  implicit class NodeOps(n: Node.Node) {
    def start: Option[Int] = Option(n.range).map(_._1)
    def end: Option[Int] = Option(n.range).map(_._2)
  }

  def nodeClassName(node: Node.Node): String = node.getClass.getSimpleName


  // short aliases (often inspired by Uglify AST names)
  type Dot = Node.StaticMemberExpression
  val Dot = Node.StaticMemberExpression

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

  def propertyValue(n: Node.ObjectExpressionProperty): Option[Node.Node] = {
    n match {
      case p: Node.Property =>
        Some(p.value)
      case _ =>
        None
    }
  }

  def keyValIsTemplate(kv: Node.Property): Boolean = propertyName(kv) startsWith Symbols.templatePrefix

  def propertyIsStatic(p: Node.ClassBodyElement): Boolean = {
    p match {
      case p: Node.MethodDefinition =>
        p.static
    }
  }

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
