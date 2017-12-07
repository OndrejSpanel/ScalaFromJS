package com.github.opengrabeso
package transform

import Uglify._
import UglifyExt._
import UglifyExt.Import._
import JsUtils._

object Modules {
  /*
  * Exports with no filename serve no purpose, and they can confuse walkers
  * We remove them */
  def cleanupExports(n: AST_Node): AST_Node = {
    n.transformAfterSimple {
      case AST_Export(Undefined(), Undefined(), Defined(value)) =>
        // sometimes we might need to wrap in a statement?
        value match {
          case value: AST_Statement =>
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

  /*
  * import in form import {* as Xxxx} from "module" are confusing - the symbols are not in fact renamed,
  * as we are merging them as plain text.
  * We fix this by removing Xxxx. prefixes
  */
  def inlineImports(n: AST_Node): AST_Node = {
    val namespacesToRemoveBuilder = Set.newBuilder[String]
    n.walk {
      case AST_Import(str, Defined(syms), _) =>
        if (syms.length == 1) {
          if (syms.head.foreign_name.name == "*") {
            namespacesToRemoveBuilder += syms.head.name.name
          }
        }
        false
      case _ =>
        false
    }
    val namespacesToRemove = namespacesToRemoveBuilder.result()
    //println(s"namespacesToRemove $namespacesToRemove")

    n.transformAfterSimple {
      case node@AST_SymbolName(x) AST_Dot name if namespacesToRemove contains x =>
        AST_SymbolRef(node)(name)
      case node =>
        node
    }
  }
}
