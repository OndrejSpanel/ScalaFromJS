package com.github.opengrabeso.esprima.symbols

import esprima.Node._

object SymbolDeclaration {
  def declaredSymbols(node: Node, parent: Option[Node]): Seq[String] = {
    val directDeclarations: Seq[Node] = node match {
      case cls: ClassDeclaration =>
        Seq(cls.id)
      case v: VariableDeclarator =>
        Seq(v.id)
      case f: FunctionDeclaration =>
        Seq(f.id)
      case _ =>
        Seq()
    }
    // some symbols are defined in the parent, like function parameters
    val parentDeclarations: Seq[Node] = parent match {
      case Some(f: FunctionDeclaration) =>
        f.params
      case Some(f: FunctionExpression) =>
        f.params
      case _ =>
        Seq()
    }

    val names = (directDeclarations ++ parentDeclarations).flatMap {
      case Identifier(id) =>
        Some(id)
      case AssignmentPattern(Identifier(id), _) =>
        Some(id)
      case _ =>
        None
    }
    names
  }
}
