package com.github.opengrabeso
package transform

import Uglify._
import UglifyExt._
import UglifyExt.Import._

object BoolComparison {
  def apply(n: AST_Node): AST_Node = {

    object IsTrue {
      def unapply(op: AST_Binary) = op match {
        case AST_Binary(expr, "!="|"!==", _: AST_False) =>
          Some(expr)
        case AST_Binary(expr, "=="|"===", _: AST_True) =>
          Some(expr)
        case _ =>
          None
      }
    }
    object IsFalse {
      def unapply(op: AST_Binary) = op match {
        case AST_Binary(expr, "=="|"===", _: AST_False) =>
          Some(expr)
        case AST_Binary(expr, "!="|"!==", _: AST_True) =>
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
          new AST_UnaryPrefix {
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
