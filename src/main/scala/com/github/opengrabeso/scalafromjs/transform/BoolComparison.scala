package com.github.opengrabeso.scalafromjs
package transform

import com.github.opengrabeso.scalafromjs.esprima._
import com.github.opengrabeso.esprima._

object BoolComparison {
  def apply(n: Node.Node): Node.Node = {

    object IsTrue {
      def unapply(op: Node.BinaryExpression) = op match {
        case Binary(expr, "!="|"!==", BooleanLiteral(false)) =>
          Some(expr)
        case Binary(expr, "=="|"===", BooleanLiteral(true)) =>
          Some(expr)
        case _ =>
          None
      }
    }
    object IsFalse {
      def unapply(op: Node.BinaryExpression) = op match {
        case Binary(expr, "=="|"===", BooleanLiteral(false)) =>
          Some(expr)
        case Binary(expr, "!="|"!==", BooleanLiteral(true)) =>
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
          Node.UnaryExpression("!", expr)
        case _ =>
          node
      }
    }
  }
}
