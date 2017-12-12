package com.github.opengrabeso
package transform

import com.github.opengrabeso.esprima._
import _root_.esprima._

object BoolComparison {
  def apply(n: Node.Node): Node.Node = {

    object IsTrue {
      def unapply(op: Node.BinaryExpression) = op match {
        case Node.BinaryExpression(expr, "!="|"!==", _: Node.False) =>
          Some(expr)
        case Node.BinaryExpression(expr, "=="|"===", _: Node.True) =>
          Some(expr)
        case _ =>
          None
      }
    }
    object IsFalse {
      def unapply(op: Node.BinaryExpression) = op match {
        case Node.BinaryExpression(expr, "=="|"===", _: Node.False) =>
          Some(expr)
        case Node.BinaryExpression(expr, "!="|"!==", _: Node.True) =>
          Some(expr)
        case _ =>
          None
      }
    }


    n.transformAfter { (node, _) =>
      node match {
        case IsTrue(expr) =>
          expr
        case IsFalse(expr) =>
          new Node.UnaryPrefix {
            fillTokens(this, node)
            this.operator = "!"
            this.expression = expr
          }
        case _ =>
          node
      }
    }
    n
  }
}
