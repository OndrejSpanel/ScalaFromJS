package com.github.opengrabeso

import Uglify._
import UglifyExt._
import UglifyExt.Import._
import JsUtils._

// extractor for special cases of the for loop
object ForRange {
  object VarOrLet {
    def unapply(arg: AST_Definitions): Option[AST_Definitions] = arg match {
      case _: AST_Var => Some(arg)
      case _: AST_Let => Some(arg)
      case _ => None
    }
  }

  def unapply(arg: AST_For): Option[(SymbolDef, String, AST_Node, AST_Node, AST_Node)] = {
    def negateStep(step: AST_Node): AST_Node = {
      new AST_UnaryPrefix {
        fillTokens(this, step)
        operator = "-"
        expression = step.clone()
      }

    }

    def createRange(vName: SymbolDef, vValue: AST_Node, rel: String, cRight: AST_Node, assign: String, step: AST_Node) = {
      (rel, assign) match {
        case ("<", "+=") =>
          Some((vName, "until", vValue, cRight, step))
        case ("<=", "+=") =>
          Some((vName, "to", vValue, cRight, step))
        case (">", "-=") =>
          Some((vName, "until", vValue, cRight, negateStep(step)))
        case (">=", "-=") =>
          Some((vName, "to", vValue, cRight, negateStep(step)))
        case _ =>
          None
      }
    }

    (arg.init.nonNull, arg.condition.nonNull, arg.step.nonNull) match {
      // for ( var i = 0; i < xxxx; i += step )
      case (
        Some(VarOrLet(AST_Definitions(AST_VarDef(AST_SymbolDef(vName), Defined(vValue))))),
        Some(AST_Binary(AST_SymbolRefName(cLeftName), rel, cRight)),
        Some(AST_Binary(AST_SymbolRefName(exprName), assign, step))
        ) if cLeftName == vName.name && exprName == vName.name =>
        createRange(vName, vValue, rel, cRight, assign, step)
      // for ( var i = 0, limit = xxxx; i < limit; i += step )
      case (
        Some(VarOrLet(AST_Definitions(AST_VarDef(AST_SymbolDef(vName), Defined(vValue)), AST_VarDef(AST_SymbolName(limitName), Defined(limitValue))))),
        Some(AST_Binary(AST_SymbolRefName(cLeftName), rel, AST_SymbolRefName(cRightName))),
        Some(AST_Binary(AST_SymbolRefName(exprName), assign, step))
        ) if cRightName == limitName && cLeftName == vName.name && exprName == vName.name =>
        createRange(vName, vValue, rel, limitValue, assign, step)

      case _ => None
    }
  }
}
