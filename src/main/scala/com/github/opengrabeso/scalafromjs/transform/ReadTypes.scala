package com.github.opengrabeso.scalafromjs
package transform

import com.github.opengrabeso.esprima._
import com.github.opengrabeso.scalafromjs.esprima.symbols.Id
import com.github.opengrabeso.scalafromjs.transform.TypesRule.typeInfoFromAST
import esprima._

/**
  * Read types from AST (typically result of TypeScript parsing) and create an accompanying type information from them */
object ReadTypes {
  def apply(n: NodeExtended): NodeExtended = {
    var types = n.types.types

    n.top.walkWithScope { (node, context) =>
      implicit val ctx = context
      // TODO: handle d.ts processing in two phases - first copy AST types from TS to JS, then process here

      def addType(name: String, t: Node.TypeAnnotation) = {
        for (tt <- typeInfoFromAST(t)(context)) {
          types += Id(name) -> tt
        }
      }
      node match {
        case Node.MethodDefinition(Node.Identifier(funName), ret, _, AnyFunEx(pars, retFun, body), _, _) => // member function
          for {
            t <- Option(ret).orElse(retFun)
          } {
            addType(funName, t)
          }
          false
        case Node.FunctionParameterWithType(Node.Identifier(name), Defined(t), defValue, optional) =>
          addType(name, t)
          false
        case Node.FunctionDeclaration(Node.Identifier(funName), params, body, _, Defined(t)) =>
          addType(funName, t)
          false
        case Node.VariableDeclarator(Node.Identifier(name), init, Defined(t)) =>
          addType(name, t)
          false
        case _ =>
          false
      }
    }
    n.copy(types = n.types.copy(types = types))
  }
}
