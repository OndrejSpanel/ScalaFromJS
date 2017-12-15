package com.github.opengrabeso.esprima.symbols

import esprima.Node._

object SymbolDeclaration {
  // some symbols should be declared in the parent scope (function name)
  def processNodes(nodes: Seq[Node]) = {
    nodes.flatMap {
      case Identifier(id) =>
        Some(id)
      case AssignmentPattern(Identifier(id), _) =>
        Some(id)
      case _ =>
        None
    }
  }

  def declaredSymbolsExtern(node: Node): Seq[String] = {
    val nodes = node match {
      case cls: ClassDeclaration =>
        Seq(cls.id)
      case v: VariableDeclarator =>
        Seq(v.id)
      case f: FunctionDeclaration =>
        Seq(f.id)
      case f: AsyncFunctionDeclaration =>
        Seq(f.id)
      case _ =>
        Seq()
    }
    processNodes(nodes)
  }

  def declaredSymbols(node: Node): Seq[String] = {
    // some symbols are defined in the parent, like function parameters
    val nodes = node match {
      case f: FunctionDeclaration =>
        f.params
      case f: FunctionExpression =>
        f.params
      case f: ArrowFunctionExpression =>
        f.params
      case f: AsyncFunctionDeclaration =>
        f.params
      case f: AsyncFunctionExpression =>
        f.params
      case f: AsyncArrowFunctionExpression =>
        f.params
      case _ =>
        Seq()
    }
    processNodes(nodes)
  }
}
