package com.github.opengrabeso

import net.gamatron.esprima._
import esprima._

import JsUtils._

// extractor for special cases of the for loop
object ForRange {
  object VarOrLet {
    def unapply(arg: Node.Definitions): Option[Node.Definitions] = arg match {
      case _: Node.Var => Some(arg)
      case _: Node.Let => Some(arg)
      case _ => None
    }
  }

  def unapply(arg: Node.For): Option[(SymbolDef, String, Node.Node, Node.Node, Node.Node)] = {
    def negateStep(step: Node.Node): Node.Node = {
      new Node.UnaryPrefix {
        fillTokens(this, step)
        operator = "-"
        expression = step.clone()
      }

    }

    def createRange(vName: SymbolDef, vValue: Node.Node, rel: String, cRight: Node.Node, assign: String, step: Node.Node) = {
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
        Some(VarOrLet(Node.Definitions(Node.VarDef(Node.SymbolDef(vName), Defined(vValue))))),
        Some(Node.Binary(Node.SymbolRefName(cLeftName), rel, cRight)),
        Some(Node.Binary(Node.SymbolRefName(exprName), assign, step))
        ) if cLeftName == vName.name && exprName == vName.name =>
        createRange(vName, vValue, rel, cRight, assign, step)
      // for ( var i = 0, limit = xxxx; i < limit; i += step )
      case (
        Some(VarOrLet(Node.Definitions(Node.VarDef(Node.SymbolDef(vName), Defined(vValue)), Node.VarDef(Node.SymbolName(limitName), Defined(limitValue))))),
        Some(Node.Binary(Node.SymbolRefName(cLeftName), rel, Node.SymbolRefName(cRightName))),
        Some(Node.Binary(Node.SymbolRefName(exprName), assign, step))
        ) if cRightName == limitName && cLeftName == vName.name && exprName == vName.name =>
        createRange(vName, vValue, rel, limitValue, assign, step)

      case _ => None
    }
  }
}
