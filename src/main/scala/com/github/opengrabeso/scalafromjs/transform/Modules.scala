package com.github.opengrabeso.scalafromjs
package transform

import com.github.opengrabeso.scalafromjs.esprima._
import com.github.opengrabeso.esprima._
import JsUtils._

object Modules {
  /*
  * Exports with no filename serve no purpose, and they can confuse walkers
  * We remove them */
  def cleanupExports(n: Node.Node): Node.Node = {
    def exportNode(value: Node.Node) = {
      // sometimes we might need to wrap in a statement?
      value match {
        case value: Node.Statement =>
          value
        case value: Node.Expression =>
          //println(s"Wrap export of $value")
          Node.ExpressionStatement(value)
        case node =>
          node
      }
    }

    n.transformAfterSimple {
      case Node.ExportDefaultDeclaration(Defined(value)) =>
        exportNode(value)
      case Node.ExportNamedDeclaration(Defined(value), _, _) =>
        exportNode(value)
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
      case Node.ImportDeclaration(Seq(Node.ImportSpecifier(Node.Identifier(local), Node.Identifier(imported))), _) =>
        if (imported == "*") {
          namespacesToRemoveBuilder += local
        }
        false
      case _ =>
        false
    }
    val namespacesToRemove = namespacesToRemoveBuilder.result()
    //println(s"namespacesToRemove $namespacesToRemove")

    n.transformAfterSimple {
      case Node.StaticMemberExpression(Node.Identifier(x), name: Node.Identifier) if namespacesToRemove contains x =>
        name
      case node =>
        node
    }
  }
}
