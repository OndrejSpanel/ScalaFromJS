package com.github.opengrabeso

import _root_.esprima.Node

trait NodeExt {

  implicit class NodeOps(n: Node.Node) {
    def start: Option[Int] = Option(n.range).map(_._1)
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
