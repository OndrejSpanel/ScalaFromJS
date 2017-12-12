package com.github.opengrabeso

import _root_.esprima.Node

trait NodeExt {

  implicit class NodeOps(n: Node.Node) {
    def start: Option[Int] = Option(n.range).map(_._1)
  }

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

  type Object = Node.ObjectExpression
  val Object = Node.ObjectExpression

  type VarDef = Node.VariableDeclarator
  val VarDef = Node.VariableDeclarator
}
