package com.github.opengrabeso.scalafromjs.esprima
package symbols

import com.github.opengrabeso.esprima.Node._
import scala.collection.Seq

object SymbolDeclaration {
  // some symbols should be declared in the parent scope (function name)
  def processNodes(nodes: Seq[(Node, Boolean)]) = {
    nodes.flatMap { case (node, member) =>
      val nodeProcessed = node match {
        case Identifier(id) =>
          Some(id)
        case FunctionParameterWithType(Identifier(id), _, _, _) =>
          Some(id)
        case AssignmentPattern(Identifier(id), _) =>
          Some(id)
        case _ =>
          None
      }

      nodeProcessed.map(_ -> member)
    }
  }

  def declaredSymbols(node: Node): Seq[(String, Boolean)] = {
    // some symbols are defined in the parent, like function parameters
    def processBlock(body: Seq[Node]): Seq[(BindingIdentifierOrPattern, Boolean)] = {
      body.flatMap {
        case f: FunctionDeclaration =>
          Seq((f.id, false))
        case f: AsyncFunctionDeclaration =>
          Seq((f.id, false))
        case MethodDefinition(id: Identifier, _, _, _, _, _) =>
          Seq((id, true))
        case v: VariableDeclaration =>
          v.declarations.map(d => (d.id, false))
        case c: ClassDeclaration =>
          Seq((c.id, false))
        case _ =>
          Seq.empty
      }
    }


    val nodes: Seq[(Node, Boolean)] = node match {
      case block: BlockStatement =>
        processBlock(block.body)
      case block: Program =>
        processBlock(block.body)
      case block: ClassBody =>
        processBlock(block.body)
      case cls: ClassDeclaration =>
        Seq((cls.id, false))
      // scan VariableDeclaration so that the declaration is already available when pattern matching against it
      case v: VariableDeclaration =>
        v.declarations.map(d => d.id -> false)
      // most often the variable is already defined from VariableDeclaration, but it does no harm to define it again
      case v: VariableDeclarator =>
        Seq(v.id -> false)
      case f: FunctionDeclaration =>
        f.params.map(_ -> false)
      case f: FunctionExpression =>
        f.params.map(_ -> false)
      case f: ArrowFunctionExpression =>
        f.params.map(_ -> false)
      case f: AsyncFunctionDeclaration =>
        f.params.map(_ -> false)
      case f: AsyncFunctionExpression =>
        f.params.map(_ -> false)
      case f: AsyncArrowFunctionExpression =>
        f.params.map(_ -> false)
      case f: MethodDefinition =>
        Seq(f.key).collect {
          case id: Identifier => id -> true
        }
      case _ =>
        Seq()
    }
    processNodes(nodes)
  }
}
