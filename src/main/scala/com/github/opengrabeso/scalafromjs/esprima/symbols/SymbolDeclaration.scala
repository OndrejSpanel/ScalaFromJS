package com.github.opengrabeso.scalafromjs.esprima
package symbols

import com.github.opengrabeso.esprima.Node._

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

  def declaredSymbols(node: Node): Seq[String] = {
    // some symbols are defined in the parent, like function parameters
    def processBlock(body: Seq[Node]): Seq[BindingIdentifierOrPattern] = {
      body.flatMap {
        case f: FunctionDeclaration =>
          Seq(f.id)
        case f: AsyncFunctionDeclaration =>
          Seq(f.id)
        case MethodDefinition(id: Identifier, _, _, _, _, _) =>
          Seq(id)
        case v: VariableDeclaration =>
          v.declarations.map(_.id)
        case c: ClassDeclaration =>
          Seq(c.id)
        case _ =>
          Seq.empty
      }
    }


    val nodes = node match {
      case block: BlockStatement =>
        processBlock(block.body)
      case block: Program =>
        processBlock(block.body)
      case block: ClassBody =>
        processBlock(block.body)
      case cls: ClassDeclaration =>
        Seq(cls.id)
      // scan VariableDeclaration so that the declaration is already available when pattern matching against it
      case v: VariableDeclaration =>
        v.declarations.map(_.id)
      // most often the variable is already defined from VariableDeclaration, but it does no harm to define it again
      case v: VariableDeclarator =>
        Seq(v.id)
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
      case f: MethodDefinition =>
        Seq(f.key).collect {
          case id: Identifier => id
        }
      case _ =>
        Seq()
    }
    processNodes(nodes)
  }
}
