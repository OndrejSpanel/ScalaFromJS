package com.github.opengrabeso
package transform

import net.gamatron.esprima._
import esprima._

import JsUtils._

object Modules {
  /*
  * Exports with no filename serve no purpose, and they can confuse walkers
  * We remove them */
  def cleanupExports(n: Node.Node): Node.Node = {
    n.transformAfterSimple {
      case Node.Export(Undefined(), Undefined(), Defined(value)) =>
        // sometimes we might need to wrap in a statement?
        value match {
          case value: Node.Statement =>
            value
          case _ =>
            println(s"Wrap export of $value")
            new Node.SimpleStatement {
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
  def inlineImports(n: Node.Node): Node.Node = {
    val namespacesToRemoveBuilder = Set.newBuilder[String]
    n.walk {
      case Node.Import(str, Defined(syms), _) =>
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
      case node@Node.SymbolName(x) Node.StaticMemberExpression name if namespacesToRemove contains x =>
        Node.SymbolRef(node)(name)
      case node =>
        node
    }
  }
}
