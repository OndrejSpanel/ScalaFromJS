package com.github.opengrabeso

import com.github.opengrabeso.Uglify._
import com.github.opengrabeso.UglifyExt.Import._

object Expressions {
  object IsConstant {
    def unapply(arg: AST_Node): Boolean = arg match {
      case _: AST_Constant => true
      case AST_SymbolRef("Infinity", _, _) => true
      case _ =>
        //println(s"Not constant $arg")
        false
    }
  }

  trait RecursiveExpressionCondition {
    def allow(c: AST_Node): Boolean

    def check(c: AST_Node): Boolean = unapply(c).isDefined

    def unapply(arg: AST_Node): Option[AST_Node] = arg match {
      case c if allow(c) =>
        Some(c)
      case AST_New(cls, args@_*) if args.forall(check) =>
        Some(arg)
      case AST_Array(args@_*) if args.forall(check) =>
        Some(arg)
      case AST_Object(props) if props.map(_.value).forall(check) =>
        Some(arg)
      case AST_Binary(a, _, b) if check(a) && check(b) =>
        Some(arg)
      case AST_Unary(UnaryModification(), _) =>
        None
      case AST_Unary(_, a) if check(a) =>
        Some(arg)
      case _ =>
        None
    }

  }

  object IsConstantInitializer extends RecursiveExpressionCondition {
    def allow(c: AST_Node): Boolean = IsConstant.unapply(c)
  }

  // a bit relaxed, some non-constant expression allowed as well
  // while this theoretically incorrect, in practice the results seem good
  // TODO: use RecursiveExpressionCondition trait instead, define relaxed requirements explicitely
  object InitStatement {
    def unapply(arg: AST_Node) = arg match {
      case c@IsConstant() => Some(c)
      // TODO: accept only some forms of new or Array (avoid reordering dependent expressions)
      case c: AST_Array => Some(c)
      case c: AST_New => Some(c)
      case c@AST_Object(Seq()) => Some(c)
      // TODO: check for dependent expressions
      case c: AST_SymbolRef => Some(c)
      case c@((x: AST_SymbolRef) AST_Dot _) => Some(c)
      case _ =>
        //println(s"not InitStatement $arg")
        None
    }
  }

  object UnaryModification {
    def unapply(arg: String): Boolean = arg == "++" || arg == "--"
  }

  object Statements {
    def unapply(arg: AST_Statement) = arg match {
      case AST_BlockStatement(body) => Some(body)
      case s@AST_SimpleStatement(body) => Some(Seq(s))
      case _ => None
    }
  }

  object SingleStatement {
    def unapply(arg: AST_Statement): Option[AST_Node] = arg match {
      case AST_BlockStatement(Seq(AST_SimpleStatement(body))) => Some(body)
      case AST_SimpleStatement(body) => Some(body)
      case _ => None
    }
  }



}
