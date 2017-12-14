package com.github.opengrabeso

import com.github.opengrabeso.esprima._
import _root_.esprima._
import JsUtils._
import com.github.opengrabeso.esprima.symbols.{Id, ScopeContext, SymId}

// extractor for special cases of the for loop
object ForRange {
  object VarOrLet {
    def unapply(arg: Node.VariableDeclaration): Option[Node.VariableDeclaration] = arg.kind match {
      case "var" => Some(arg)
      case "let" => Some(arg)
      case _ => None
    }
  }

  def unapply(arg: Node.ForStatement)(implicit context: ScopeContext): Option[(SymId, String, Node.Node, Node.Node, Node.Node)] = {
    def negateStep(step: Node.Expression): Node.Node = {
      Node.UnaryExpression("-", step.clone().asInstanceOf[Node.Expression]).copyNode(step)
    }

    def createRange(vName: SymId, vValue: Node.Node, rel: String, cRight: Node.Node, assign: String, step: Node.Expression) = {
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

    (Option(arg.init), Option(arg.test), Option(arg.update)) match {
      // for ( var i = 0; i < xxxx; i += step )
      case (
        Some(VarOrLet(Node.VariableDeclaration(Seq(Node.VariableDeclarator(Node.Identifier(Id(vName)), Defined(vValue))), _))),
        Some(Node.BinaryExpression(rel, Node.Identifier(Id(cLeftName)), cRight)),
        Some(Node.AssignmentExpression(assign, Node.Identifier(Id(exprName)), step))
        ) if cLeftName == vName && exprName == vName =>
        createRange(vName, vValue, rel, cRight, assign, step)
      // for ( var i = 0, limit = xxxx; i < limit; i += step )
      case (
        Some(VarOrLet(Node.VariableDeclaration(
          Seq(Node.VariableDeclarator(Node.Identifier(Id(vName)), Defined(vValue)), Node.VariableDeclarator(Node.Identifier(Id(limitName)), Defined(limitValue))), _
        ))),
        Some(Node.BinaryExpression(rel, Node.Identifier(Id(cLeftName)), Node.Identifier(Id(cRightName)))),
        Some(Node.AssignmentExpression(assign, Node.Identifier(Id(exprName)), step))
        ) if cRightName == limitName && cLeftName == vName && exprName == vName =>
        createRange(vName, vValue, rel, limitValue, assign, step)

      case _ => None
    }
  }
}
