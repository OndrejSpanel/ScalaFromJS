package com.github.opengrabeso.scalafromjs

import com.github.opengrabeso.esprima.Node
import esprima.walker

import scala.reflect.runtime.universe.{Literal => _, _}

object ScalaNode {
  // Scala specific node extensions for the JS AST tree
  case class StatementExpression(var statement: Node.Statement) extends Node.Node with Node.Expression {

    override def clone() = copy()
  }

  case class MemberTemplate(name: String, original: Node.ClassBodyElement, value: String) extends Node.Node with Node.ClassBodyElement {
    override def clone() = copy()
  }

  walker.addNodeTypes(typeOf[ScalaNode.type])

  def apply() = {}

}
