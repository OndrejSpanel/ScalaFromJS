package com.github.opengrabeso.esprima

import com.github.opengrabeso.esprima.walker.addNodeTypes
import esprima.{Esprima, Node}
import org.scalatest.FunSuite
import esprima.Esprima._

import scala.reflect.runtime.universe._

object Extension {
  case class StatementAsExpression(var statement: Node.Statement) extends Node.Node with Node.Expression {
    def `type` = "StatementAsExpression"

    override def clone() = copy()
  }
}

class ExtensionsTest extends FunSuite with TestInputs {
  test("Extend AST types with custom nodes") {

    addNodeTypes(typeOf[Extension.type])

    val ast = parse("answer = 42")


    val transformed = ast.transformBefore {(node, descend, transformer) =>
      node match {
        case node: Node.Literal =>
          Extension.StatementAsExpression(Node.BlockStatement(Seq(Node.ExpressionStatement(node.cloneNode()))))
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
