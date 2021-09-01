package com.github.opengrabeso.scalafromjs.esprima

import com.github.opengrabeso.esprima.Node
import org.scalatest.funsuite.AnyFunSuite
import com.github.opengrabeso.esprima.walker

object Extension {
  case class StatementAsExpression(var statement: Node.Statement) extends Node.Node with Node.Expression {
    def `type` = "StatementAsExpression"

    override def clone() = copy()
  }
}

class ExtensionsTest extends AnyFunSuite with TestInputs {
  test("Extend AST types with custom nodes") {

    import walker._
    walker.addNodeTypes[Node.Node, Extension.type](walker.Walker)

    val ast = parse("answer = 42")


    val transformed = ast.transformBefore {(node, descend, transformer) =>
      node match {
        case node: Node.Literal =>
          Extension.StatementAsExpression(Node.BlockStatement(Seq(Node.ExpressionStatement(node.cloneNode()))).withTokens(node))
        case _ =>
          descend(node.cloneNode(), transformer)
      }
    }

    var containsBlock = false
    transformed.walk {
      case _: Node.BlockStatement =>
        containsBlock = true
        false
      case _ =>
        false

    }
    assert(containsBlock)
  }


}
