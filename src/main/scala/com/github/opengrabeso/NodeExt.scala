package com.github.opengrabeso

import esprima.Node

object NodeExt {

  implicit class NodeOps(n: Node.Node) {
    def start: Option[Int] = Option(n.range).map(_._1)
  }

  // short aliases (often inspired by Uglify AST names)
  type Dot = Node.StaticMemberExpression
  val Dot = Node.StaticMemberExpression

  type Conditional = Node.ConditionalExpression
  val Conditional = Node.ConditionalExpression

  type Array = Node.ArrayExpression
  val Array = Node.ArrayExpression

  type Object = Node.ObjectExpression
  val Object = Node.ObjectExpression

  type VarDef = Node.VariableDeclarator
  val VarDef = Node.VariableDeclarator
}
