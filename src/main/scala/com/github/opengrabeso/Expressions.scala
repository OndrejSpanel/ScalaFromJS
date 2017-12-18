package com.github.opengrabeso

import com.github.opengrabeso.esprima._
import _root_.esprima._

object Expressions {
  object IsConstant {
    def unapply(arg: Node.Node): Boolean = arg match {
      case _: Node.Literal => true
      case Node.Identifier("Infinity") => true
      case _ =>
        //println(s"Not constant $arg")
        false
    }
  }

  trait RecursiveExpressionCondition {
    def allow(c: Node.Node): Boolean
    def forbid(c: Node.Node): Boolean

    def check(c: Node.Node): Boolean = unapply(c).isDefined

    def unapply(arg: Node.Node): Option[Node.Node] = arg match {
      case c if allow(c) =>
        Some(c)
      case c if forbid(c) =>
        None
      case Node.NewExpression(cls, args) if args.forall(check) =>
        Some(arg)
      case AArray(args) if args.forall(check) =>
        Some(arg)
      case OObject(props) if props.flatMap(propertyValue).forall(check) =>
        Some(arg)
      case Node.BinaryExpression(_, a, b) if check(a) && check(b) =>
        Some(arg)
      case Node.UnaryExpression(_, a) if check(a) =>
        Some(arg)
      case _ =>
        None
    }

  }

  object IsConstantInitializer extends RecursiveExpressionCondition {
    def allow(c: Node.Node): Boolean = IsConstant.unapply(c)
    def forbid(c: Node.Node): Boolean = c match {
      case _ : Node.UpdateExpression =>
        true
      case _ : Node.AssignmentExpression =>
        true
    }
  }

  // a bit relaxed, some non-constant expression allowed as well
  // while this theoretically incorrect, in practice the results seem good
  // TODO: use RecursiveExpressionCondition trait instead, define relaxed requirements explicitely
  object InitStatement {
    def unapply(arg: Node.Expression) = arg match {
      case c@IsConstant() => Some(c)
      // TODO: accept only some forms of new or Array (avoid reordering dependent expressions)
      case c: AArray => Some(c)
      case c: Node.NewExpression => Some(c)
      case c@OObject(Seq()) => Some(c)
      // TODO: check for dependent expressions
      case c: Node.Identifier => Some(c)
      case c@((x: Node.Identifier) Dot _) => Some(c)
      case _ =>
        //println(s"not InitStatement $arg")
        None
    }
  }

  object UnaryModification {
    def unapply(arg: String): Boolean = arg == "++" || arg == "--"
  }

  object Statements {
    def unapply(arg: Node.Statement) = arg match {
      case Node.BlockStatement(body) => Some(body)
      case s@Node.ExpressionStatement(body) => Some(Seq(s))
      case _ => None
    }
  }

  object SingleExpression {
    def unapply(arg: Node.Statement): Option[Node.Node] = arg match {
      case Node.BlockStatement(Seq(Node.ExpressionStatement(body))) => Some(body)
      case Node.ExpressionStatement(body) => Some(body)
      case _ => None
    }
  }



}
