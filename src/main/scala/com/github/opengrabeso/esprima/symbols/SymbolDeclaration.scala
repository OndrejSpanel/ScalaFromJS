package com.github.opengrabeso.esprima.symbols

import esprima.Node._

object SymbolDeclaration {
  def declaredSymbols(node: Node): Seq[String] = {
    val declarations: Seq[Node] = node match {
      case cls: ClassDeclaration =>
        Seq(cls.id)
      case v: VariableDeclarator =>
        Seq(v.id)
      case f: FunctionDeclaration =>
        // TODO: parameters into a different scope
        f.id +: f.params
      case f: FunctionExpression =>
        f.id +: f.params
      case _ =>
        Seq()
    }
    val names = declarations.flatMap {
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
