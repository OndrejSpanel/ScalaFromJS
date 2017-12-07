package com.github.opengrabeso
package transform

import Uglify._
import UglifyExt._
import UglifyExt.Import._

import scala.scalajs.js

object CleanupExports {
  /*
  * Exports with no filename serve no purpose, and they can confuse walkers
  * We remove them */
  def apply(n: AST_Node): AST_Node = {
    n.transformAfterSimple {
      case ex@AST_Export(Undefined(), Undefined(), Defined(value)) =>
        // sometimes we might need to wrap in a statement?
        value match {
          case _: AST_Statement =>
            value
          case _ =>
            println(s"Wrap export of $value")
            new AST_SimpleStatement {
              fillTokens(this, value)
              body = value
            }
        }
      case node =>
        node

    }
  }
}
