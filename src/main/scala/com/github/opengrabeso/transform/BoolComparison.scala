package com.github.opengrabeso
package transform

import net.gamatron.esprima._
import esprima._

object BoolComparison {
  def apply(n: Node.Node): Node.Node = {

    object IsTrue {
      def unapply(op: Node.Binary) = op match {
        case Node.Binary(expr, "!="|"!==", _: Node.False) =>
          Some(expr)
        case Node.Binary(expr, "=="|"===", _: Node.True) =>
          Some(expr)
        case _ =>
          None
      }
    }
    object IsFalse {
      def unapply(op: Node.Binary) = op match {
        case Node.Binary(expr, "=="|"===", _: Node.False) =>
          Some(expr)
        case Node.Binary(expr, "!="|"!==", _: Node.True) =>
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
