package com.github.opengrabeso

import com.github.opengrabeso.esprima._
import _root_.esprima._

import JsUtils._

// extractor for special cases of the for loop
object ForRange {
  object VarOrLet {
    def unapply(arg: Node.VariableDeclaration): Option[Node.VariableDeclaration] = arg match {
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

    (arg.init, arg.condition, arg.step) match {
      // for ( var i = 0; i < xxxx; i += step )
      case (
        Some(VarOrLet(Node.VariableDeclaration(Node.VariableDeclarator(Node.Identifier(vName), Defined(vValue))))),
        Some(Node.BinaryExpression(Node.Identifier(cLeftName), rel, cRight)),
        Some(Node.BinaryExpression(Node.Identifier(exprName), assign, step))
        ) if cLeftName == vName.name && exprName == vName.name =>
        createRange(vName, vValue, rel, cRight, assign, step)
      // for ( var i = 0, limit = xxxx; i < limit; i += step )
      case (
        Some(VarOrLet(Node.VariableDeclaration(Node.VariableDeclarator(Node.Identifier(vName), Defined(vValue)), Node.VariableDeclarator(Node.SymbolName(limitName), Defined(limitValue))))),
        Some(Node.BinaryExpression(Node.Identifier(cLeftName), rel, Node.Identifier(cRightName))),
        Some(Node.BinaryExpression(Node.Identifier(exprName), assign, step))
        ) if cRightName == limitName && cLeftName == vName.name && exprName == vName.name =>
        createRange(vName, vValue, rel, limitValue, assign, step)

      case _ => None
    }
  }
}
