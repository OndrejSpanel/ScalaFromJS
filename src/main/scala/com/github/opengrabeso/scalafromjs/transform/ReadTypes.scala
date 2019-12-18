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

    def handleParameters(pars: Seq[Node.FunctionParameter])(implicit context: symbols.ScopeContext) = {
      for {
        Node.FunctionParameterWithType(Node.Identifier(parName), Defined(t), defValue, optional) <- pars
        tt <- typeInfoFromAST(t)(context)
      } {
        types += Id(parName) -> tt
      }
    }

    n.top.walkWithScope { (node, context) =>
      implicit val ctx = context
      // processing similar to TypesRule handleClass / handleFunction, but simpler - we alredy are in a correct scope
      // TODO: handle d.ts processing in two phases - first copy AST types from TS to JS, then process here
      node match {
        case Node.MethodDefinition(Node.Identifier(funName), ret, _, AnyFunEx(pars, retFun, body), _, _) => // member function
          for {
            t <- Option(ret).orElse(retFun)
            tt <- typeInfoFromAST(t)(context)
          } {
            types += Id(funName) -> tt
          }
          false
        case Node.FunctionParameterWithType(Node.Identifier(name), Defined(t), defValue, optional) =>
          for (tt <- typeInfoFromAST(t)(context)) {
            types += Id(name) -> tt
          }
          false
        case Node.FunctionDeclaration(funName, params, body, _, Defined(t)) =>
          for (tt <- typeInfoFromAST(t)(context)) {
            types += Id(funName) -> tt
          }
          false // there may be some inner functions in there - process them as well
        case Node.VariableDeclarator(Node.Identifier(name), init, Defined(t)) =>
          for (tt <- typeInfoFromAST(t)(context)) {
            types += Id(name) -> tt
          }
          false // initialize may contain more variables - process them as well
        case _ =>
          false
      }
    }
    n.copy(types = n.types.copy(types = types))
  }
}
