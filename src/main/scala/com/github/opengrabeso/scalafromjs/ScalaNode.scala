package com.github.opengrabeso.scalafromjs

import com.github.opengrabeso.esprima.Node
import esprima.walker
import esprima.walker._

object ScalaNode {
  // Scala specific node extensions for the JS AST tree
  case class StatementExpression(var statement: Node.Statement) extends Node.Node with Node.Expression {

    override def clone() = copy()
  }

  case class MemberTemplate(var name: String, var original: Node.ClassBodyElement, var value: String) extends Node.Node with Node.ClassBodyElement {
    override def clone() = copy()
  }

  walker.allWalkers ++= walker.specializedWalkers(walker.createWalkers[Node.Node, Node.type])
  walker.allTransformers ++= walker.specializedTransformers(walker.createTransformers[Node.Node, Node.type])

  walker.allWalkers ++= walker.createWalkers[Node.Node, ScalaNode.type]
  walker.allTransformers ++= walker.createTransformers[Node.Node, ScalaNode.type]

  def apply() = {}

}
