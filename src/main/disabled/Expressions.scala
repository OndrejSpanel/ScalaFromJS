package com.github.opengrabeso

import com.github.opengrabeso.esprima._
import _root_.esprima._

object Expressions {
  object IsConstant {
    def unapply(arg: Node.Node): Boolean = arg match {
      case _: Node.Constant => true
      case Node.Identifier("Infinity", _, _) => true
      case _ =>
        //println(s"Not constant $arg")
        false
    }
  }

  trait RecursiveExpressionCondition {
    def allow(c: Node.Node): Boolean

    def check(c: Node.Node): Boolean = unapply(c).isDefined

    def unapply(arg: Node.Node): Option[Node.Node] = arg match {
      case c if allow(c) =>
        Some(c)
      case Node.New(cls, args@_*) if args.forall(check) =>
        Some(arg)
      case Node.AArray(args@_*) if args.forall(check) =>
        Some(arg)
      case Node.Object(props) if props.map(_.value).forall(check) =>
        Some(arg)
      case Node.BinaryExpression(a, _, b) if check(a) && check(b) =>
        Some(arg)
      case Node.UnaryExpression(UnaryModification(), _) =>
        None
      case Node.UnaryExpression(_, a) if check(a) =>
        Some(arg)
      case _ =>
        None
    }

  }

  object IsConstantInitializer extends RecursiveExpressionCondition {
    def allow(c: Node.Node): Boolean = IsConstant.unapply(c)
  }

  // a bit relaxed, some non-constant expression allowed as well
  // while this theoretically incorrect, in practice the results seem good
  // TODO: use RecursiveExpressionCondition trait instead, define relaxed requirements explicitely
  object InitStatement {
    def unapply(arg: Node.Node) = arg match {
      case c@IsConstant() => Some(c)
      // TODO: accept only some forms of new or Array (avoid reordering dependent expressions)
      case c: Node.AArray => Some(c)
      case c: Node.New => Some(c)
      case c@Node.Object(Seq()) => Some(c)
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
      case s@Node.SimpleStatement(body) => Some(Seq(s))
      case _ => None
    }
  }

  object SingleStatement {
    def unapply(arg: Node.Statement): Option[Node.Node] = arg match {
      case Node.BlockStatement(Seq(Node.SimpleStatement(body))) => Some(body)
      case Node.SimpleStatement(body) => Some(body)
      case _ => None
    }
  }



}
