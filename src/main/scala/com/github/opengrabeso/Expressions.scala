package com.github.opengrabeso

import com.github.opengrabeso.Uglify._
import com.github.opengrabeso.UglifyExt.Import._

object Expressions {
  object IsConstant {
    def unapply(arg: AST_Node): Option[AST_Node] = arg match {
      case c: AST_Constant => Some(arg)
      case _ => None
    }
  }

  object IsConstantInitializer {
    private def allConstants(seq: Seq[AST_Node]): Boolean = seq.forall(IsConstantInitializer.unapply(_).isDefined)
    def unapply(arg: AST_Node): Option[AST_Node] = arg match {
      case IsConstant(c) =>
        Some(c)
      case AST_New(cls, args@_*) if allConstants(args) =>
        Some(arg)
      case AST_Array(args@_*) if allConstants(args) =>
        Some(arg)
      case c@AST_Object(Seq()) => // empty object/map initializer
        Some(c)
      case _ =>
        None
    }
  }

  // a bit relaxed, some non-constant expression allowed as well
  // while this theoretically incorrect, in practice the results seem good
  object InitStatement {
    def unapply(arg: AST_Node) = arg match {
      case IsConstant(c) => Some(c)
      // TODO: accept only some forms of new or Array (avoid reordering dependent expressions)
      case c: AST_Array => Some(c)
      case c: AST_New => Some(c)
      case c@AST_Object(Seq()) => Some(c)
      // TODO: check for dependent expressions
      case c: AST_SymbolRef => Some(c)
      case c@((x: AST_SymbolRef) AST_Dot _) => Some(c)
      case _ =>
        //println(s"${nodeClassName(arg)}")
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
