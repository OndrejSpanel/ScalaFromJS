package com.github.opengrabeso.scalafromjs
package transform

import com.github.opengrabeso.esprima._
import com.github.opengrabeso.scalafromjs.esprima.symbols._
import com.github.opengrabeso.scalafromjs.transform.TypesRule.{typeFromAST, typeInfoFromAST}
import esprima._

import scala.collection.Seq

/**
  * Read types from AST (typically result of TypeScript parsing) and create an accompanying type information from them */
object ReadTypes {

  def apply(n: NodeExtended): NodeExtended = {
    var types = n.types

    // handle d.ts processing in two phases - first copy AST types from TS to JS (done in [[TypesRule]])
    // then process JS here
    n.top.walkWithScope { (node, context) =>
      implicit val ctx = context

      def addType(name: String, t: Node.TypeAnnotation) = {
        for (tt <- typeInfoFromAST(t)(context)) {
          types = types add Id(name) -> tt
        }
      }
      def addMemberType(name: String, t: Node.TypeAnnotation) = {
        for (tt <- typeInfoFromAST(t)(context)) {
          types = types add MemberFunId(name) -> tt
        }
      }
      node match {
        case Node.MethodDefinition(Node.Identifier(funName), tpe, _, funex@AnyFunEx(pars, retFun, body), kind, _) => // member function
          val memberFun = SymbolTypes.isMemberCall(kind)
          val sid = if (memberFun) memberFunId(funName).get else symId(funName).get
          val tt = retFun.flatMap(typeFromAST)
          types = types.handleParameterTypes(sid, tt, pars, pars, symbols.ScopeContext.getNodeId(funex))
          for (t <- Option(tpe).orElse(retFun)) {
            addMemberType(funName, t)
          }
          false
        case Node.MethodDefinition(Node.Identifier(funName), Defined(t), _, _, "value", _) => // member
          addMemberType(funName, t)
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
    n.copy(types = types)
  }
}
